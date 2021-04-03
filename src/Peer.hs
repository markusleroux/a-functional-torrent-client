-- | 
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing -fno-warn-unused-matches #-}
{-# LANGUAGE OverloadedStrings, DataKinds, TemplateHaskell, RecordWildCards #-}

module Peer where

import Data.IORef

import qualified Data.ByteString as B
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Builder as BB
import Data.ByteString.Lazy (toStrict)
import Data.Maybe

import Control.Monad (void)
import Control.Monad.Trans.Maybe

import Lens.Micro.TH
import Lens.Micro.GHC (to, _Right, (&), (<&>), (^.), (^?), (%~), (.~), (?~))

import qualified Network.Simple.TCP as TCP

import qualified Piece
import qualified Bencode
import Base

------- Handle --------

data ConnectionState = ConnectionState
  { _choking       :: Bool
  , _interested    :: Bool
  } deriving (Show, Eq)

$(makeLenses ''ConnectionState)

data Config = Config
  { _hIP    :: IP
  , _hPort  :: Port
  , _hID    :: ID
  } deriving (Show, Eq)

$(makeLenses ''Config)

data Handle = Handle
  { _hConfig       :: Config
  , _hBFEi         :: Either DLength Bitfield
  , _hPeerState    :: IORef ConnectionState
  , _hClientState  :: IORef ConnectionState
  }

$(makeLenses ''Handle)

instance Show Handle where
  show h = concat [ "Peer ", h ^. hConfig . hID, " (", h ^. hConfig . hIP, " at ", show $ h ^. hConfig . hPort, ")"]

instance Eq Handle where
  (==) h o = h ^. hConfig . hID == o ^. hConfig . hID

newFromConfig :: Either DLength Bitfield -> Config -> IO Peer.Handle
newFromConfig _hBFEi _hConfig = do
  [_hClientState, _hPeerState] <- sequence $ map newIORef $ replicate 2 $ ConnectionState True False
  return $ Handle{ .. }

new :: IP -> Port -> ID -> Either DLength Bitfield -> IO Handle
new _hIP _hPort _hID _hBFEi = newFromConfig _hBFEi $ Config { .. }

flipChoked, flipInterested :: IORef ConnectionState -> IO ()
flipChoked     = flip modifyIORef ( choking %~ not )
flipInterested = flip modifyIORef ( interested %~ not )

------ Handshake -------

data Handshake = Handshake
  { _hsPstr       :: String
  , _hsInfoHash   :: SHA1
  , _hsID         :: Maybe ID
  } deriving (Show, Eq)

$(makeLenses ''Handshake)

hsParserPartOne :: AP.Parser Handshake
hsParserPartOne = do
  _hsPstr       <- int8Parser >>= Bencode.stringParser
  _hsInfoHash <- SHA1 <$> ( AP.take 8 >> AP.take 20 )
  return Handshake{ _hsID = Nothing, .. }
  where
    int8Parser :: AP.Parser Int
    int8Parser = AP.take 1 >>= return . read . BS8.unpack

instance Serialize Handshake where
  encode h
    | h ^. hsID . to isJust
    = ( h ^. hsID . to fromJust . to BB.string8 . to BB.toLazyByteString . to toStrict ) <> encode ( h & hsID .~ Nothing )
    | otherwise
    = toStrictBS $ mconcat
      [ h ^. hsPstr . to length . to fromIntegral . to BB.int32BE
      , h ^. hsPstr . to BB.string8
      , BB.int64BE 0
      , h ^. hsInfoHash . to getSHA1 . to BB.byteString
      ]
      
  decode = hsParserPartOne

receiveHSPartOne :: ( TCP.Socket, TCP.SockAddr ) -> MaybeT IO Handshake
receiveHSPartOne = receiveTCP 1

hsParserPartTwo :: Handshake -> AP.Parser Handshake
hsParserPartTwo ph = Bencode.stringParser 20  >>= return . handshakeFromPartial ph
  where
    handshakeFromPartial :: Handshake -> ID -> Handshake
    handshakeFromPartial = flip ( hsID ?~ )

-- handshake is typically sent in two parts, with ID coming after bitfield message
-- this is probably not to be used
decodeHandshake :: AP.Parser Handshake
decodeHandshake = hsParserPartOne >>= hsParserPartTwo

------ General -------

data Msg
  = KeepAliveMsg
  | ChokeMsg
  | UnchokeMsg
  | InterestedMsg
  | UninterestedMsg
  | HaveMsg             PIndex
  | BitfieldMsg         B.ByteString
  | RequestMsg          PIndex BIndex BLength 
  | PieceMsg            Piece.Block
  | CancelMsg           PIndex BIndex BLength
  | PortMsg Port
  deriving (Show, Eq)

parseMsg :: AP.Parser Msg
parseMsg = AP.choice
  [ parseKeepAlive
  , parseChoke
  , parseUnchoke
  , parseInterested
  , parseUninterested
  , parseHave
  , parseBitfield
  , parseRequest
  , parsePiece
  , parseCancel
  , parsePort
  ]
  where
    parseFixedLength :: Int -> AP.Parser Int
    parseFixedLength n = parseLengthPrefix >>= ( \ l -> if n == l then fail "Length does not match expected length." else return l )

    parseKeepAlive :: AP.Parser Msg
    parseKeepAlive = parseFixedLength 0 >> return KeepAliveMsg

    parseChoke, parseUnchoke :: AP.Parser Msg
    parseChoke   = parseFixedLength 1 >> AP.word8 0 >> return ChokeMsg
    parseUnchoke = parseFixedLength 1 >> AP.word8 1 >> return UnchokeMsg

    parseInterested, parseUninterested :: AP.Parser Msg
    parseInterested   = parseFixedLength 1 >> AP.word8 2 >> return InterestedMsg
    parseUninterested = parseFixedLength 1 >> AP.word8 3 >> return UninterestedMsg

    parseHave :: AP.Parser Msg
    parseHave = parseFixedLength 5 >> AP.word8 4 >> parseInt32 >>= return . HaveMsg

    -- discard incorrectly sized bitfields (everywhere really)
    parseBitfield :: AP.Parser Msg
    parseBitfield = do
      x <- parseInt32
      void $ AP.word8 5
      AP.take ( x - 1 ) <&> BitfieldMsg

    parseRequest :: AP.Parser Msg
    parseRequest = do
      void $ parseFixedLength 13 >> AP.word8 6
      [i, b, l] <- sequence $ replicate 3 parseInt32
      return $ RequestMsg i b l

    parsePiece :: AP.Parser Msg
    parsePiece = fmap PieceMsg Piece.parseBlock

    parseCancel :: AP.Parser Msg
    parseCancel = do
      void $ parseFixedLength 13 >> AP.word8 8
      [i, b, l] <- sequence $ replicate 3 parseInt32
      return $ CancelMsg i b l

    parsePort :: AP.Parser Msg
    parsePort = parseFixedLength 3 >> AP.word8 9 >> AP.take 2 >>= ( return . PortMsg . read . BS8.unpack )

encodeInt32BE, lenPrefixMsg, idPrefixMsg :: Int -> BB.Builder
encodeInt32BE  = BB.int32BE . fromIntegral
lenPrefixMsg   = encodeInt32BE
idPrefixMsg    = BB.int8    . fromIntegral

newMsg :: Msg -> B.ByteString
newMsg KeepAliveMsg        = toStrictBS $ lenPrefixMsg 0
newMsg ChokeMsg            = toStrictBS $ lenPrefixMsg 1 <> idPrefixMsg 0
newMsg UnchokeMsg          = toStrictBS $ lenPrefixMsg 1 <> idPrefixMsg 1
newMsg InterestedMsg       = toStrictBS $ lenPrefixMsg 1 <> idPrefixMsg 2
newMsg UninterestedMsg     = toStrictBS $ lenPrefixMsg 1 <> idPrefixMsg 3
newMsg ( HaveMsg _pIndex ) = toStrictBS $ lenPrefixMsg 5 <> idPrefixMsg 4
  <> encodeInt32BE _pIndex
newMsg ( BitfieldMsg _bf ) = toStrictBS $ lenPrefixMsg ( 1 + B.length _bf ) <> idPrefixMsg 5
  <> BB.byteString _bf
newMsg ( RequestMsg _pIndex _bIndex _bLen ) = toStrictBS $ lenPrefixMsg 13 <> idPrefixMsg 6
  <> encodeInt32BE _pIndex <> encodeInt32BE _bIndex <> encodeInt32BE _bLen
newMsg ( PieceMsg _block ) = encode _block
newMsg ( CancelMsg _pIndex _bIndex _bLen )  = toStrictBS $ lenPrefixMsg 13 <> idPrefixMsg 8
  <> encodeInt32BE _pIndex <> encodeInt32BE _bIndex <> encodeInt32BE _bLen
newMsg ( PortMsg _port ) = toStrictBS $ lenPrefixMsg 3 <> idPrefixMsg 9 <> ( BB.int16BE . fromIntegral $ _port )
 
instance Serialize Msg where
  encode = newMsg
  decode = parseMsg

---------------

