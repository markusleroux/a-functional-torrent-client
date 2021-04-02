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
import Data.Array
import Data.Maybe

import Control.Monad (void)

import Lens.Micro.TH
import Lens.Micro.GHC (to, (&), (<&>), (^.), (%~), (.~), (?~))

import qualified Piece
import qualified Bencode
import Types

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
  , _hBFEi         :: Either DLength ( Array Int Bool )
  , _hPeerState    :: IORef ConnectionState
  , _hClientState  :: IORef ConnectionState
  }

$(makeLenses ''Handle)

instance Show Handle where
  show h = concat [ "Peer ", h ^. hConfig . hID, " (", h ^. hConfig . hIP, " at ", show $ h ^. hConfig . hPort, ")"]

instance Eq Handle where
  (==) h o = h ^. hConfig . hID == o ^. hConfig . hID

newFromConfig :: Either DLength ( Array Int Bool ) -> Config -> IO Peer.Handle
newFromConfig _hBFEi _hConfig = do
  [_hClientState, _hPeerState] <- sequence $ map newIORef $ replicate 2 $ ConnectionState True False
  return $ Handle{ .. }

new :: IP -> Port -> ID -> Either DLength ( Array Int Bool ) -> IO Handle
new _hIP _hPort _hID _hBFEi = newFromConfig _hBFEi $ Config { .. }

flipChoked, flipInterested :: IORef ConnectionState -> IO ()
flipChoked     = flip modifyIORef ( choking %~ not )
flipInterested = flip modifyIORef ( interested %~ not )

------ Handshake -------

data Handshake = Handshake
  { _pstr         :: String
  , _hsInfoHash   :: Piece.SHA1
  , _hsPeerID     :: Maybe ID
  } deriving (Show, Eq)

$(makeLenses ''Handshake)

instance Serialize Handshake where
  encode h
    | h ^. hsPeerID . to isJust
    = encode ( h & hsPeerID .~ Nothing ) <> ( h ^. hsPeerID . to fromJust . to BB.string8 . to BB.toLazyByteString . to toStrict )
    | otherwise
    = toStrict . BB.toLazyByteString $ mconcat
      [ h ^. pstr . to length . to fromIntegral . to BB.int32BE
      , h ^. pstr . to BB.string8
      , BB.int64BE 0
      , h ^. hsInfoHash . to Piece.getSHA1 . to BB.byteString
      ]

handshakeFromPartial :: Handshake -> String -> Handshake
handshakeFromPartial = flip ( hsPeerID ?~ )

parseHandshakePartOne :: AP.Parser Handshake
parseHandshakePartOne = do
  _pstr       <- int8Parser >>= Bencode.stringParser
  _hsInfoHash <- Piece.SHA1 <$> ( AP.take 8 >> AP.take 20 )
  return Handshake{ _hsPeerID = Nothing, ..}
  where
    int8Parser :: AP.Parser Int
    int8Parser = AP.take 1 >>= return . read . BS8.unpack

parseHandshakePartTwo :: Handshake -> AP.Parser Handshake
parseHandshakePartTwo ph = Bencode.stringParser 20  >>= return . handshakeFromPartial ph

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
  | PortMsg Int
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
    parseLength :: AP.Parser Int
    parseLength = AP.take 4 >>= ( return . read . BS8.unpack )

    parseFixedLength :: Int -> AP.Parser Int
    parseFixedLength n = parseLength >>= ( \ l -> if n == l then fail "Length does not match expected length." else return l )

    parseKeepAlive :: AP.Parser Msg
    parseKeepAlive = parseFixedLength 0 >> return KeepAliveMsg

    parseChoke, parseUnchoke :: AP.Parser Msg
    parseChoke   = parseFixedLength 1 >> AP.word8 0 >> return ChokeMsg
    parseUnchoke = parseFixedLength 1 >> AP.word8 1 >> return UnchokeMsg

    parseInterested, parseUninterested :: AP.Parser Msg
    parseInterested   = parseFixedLength 1 >> AP.word8 2 >> return InterestedMsg
    parseUninterested = parseFixedLength 1 >> AP.word8 3 >> return UninterestedMsg

    parseHave :: AP.Parser Msg
    parseHave = parseFixedLength 5 >> AP.word8 4 >> parseLength >>= return . HaveMsg

    -- discard incorrectly sized bitfields (everywhere really)
    parseBitfield :: AP.Parser Msg
    parseBitfield = do
      x <- parseLength
      void $ AP.word8 5
      AP.take ( x - 1 ) <&> BitfieldMsg

    parseRequest :: AP.Parser Msg
    parseRequest = do
      void $ parseFixedLength 13 >> AP.word8 6
      [i, b, l] <- sequence $ replicate 3 parseLength
      return $ RequestMsg i b l

    parsePiece :: AP.Parser Msg
    parsePiece = do
      _bLength  <- parseLength
      void $ AP.word8 7
      _pIndex <- parseLength
      _bIndex <- parseLength
      _bData  <- AP.take ( _bLength - 9 )
      return $ PieceMsg Piece.Block{..}

    parseCancel :: AP.Parser Msg
    parseCancel = do
      void $ parseFixedLength 13 >> AP.word8 8
      [i, b, l] <- sequence $ replicate 3 parseLength
      return $ CancelMsg i b l

    parsePort :: AP.Parser Msg
    parsePort = parseFixedLength 3 >> AP.word8 9 >> AP.take 2 >>= ( return . PortMsg . read . BS8.unpack )
    
