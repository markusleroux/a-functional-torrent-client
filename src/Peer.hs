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

import Control.Monad (void)

import Lens.Micro.TH
import Lens.Micro.GHC ((<&>), (^.), (.~))

import qualified Piece
import qualified Bencode

----------------

data ConnectionState = ConnectionState
  { _choking       :: Bool
  , _interested    :: Bool
  } deriving (Show, Eq)

$(makeLenses ''ConnectionState)

type IP = String

data Config = Config
  { _hIP   :: IP
  , _hPort  :: Int
  , _hID    :: String
  } deriving (Show, Eq)

$(makeLenses ''Config)

data Handle = Handle
  { _hConfig       :: Config
  , _hPeerState    :: IORef ConnectionState
  , _hClientState  :: IORef ConnectionState
  }

$(makeLenses ''Handle)

instance Show Handle where
  show h = concat [ "Peer ", h ^. hConfig . hID, " (", h ^. hConfig . hIP, " at ", show $ h ^. hConfig . hPort, ")"]

instance Eq Handle where
  (==) h o = h ^. hConfig . hID == o ^. hConfig . hID

new :: IP -> Int -> String -> IO Handle
new ip port id = do
  [_hClientState, _hPeerState] <- sequence $ map newIORef $ replicate 2 $ ConnectionState True False
  return $ Handle { _hConfig = Config { _hID = id, _hIP = ip, _hPort = port }, ..}

newFromConfig :: Peer.Config -> IO Peer.Handle
newFromConfig config = new ( config ^. hIP ) ( config ^. hPort ) ( config ^. hID )

class Serialize a where
  encode :: a -> B.ByteString

data PartialHandshake = PartialHandshake
  { _pstrPartial        :: String
  , _hsInfoHashPartial  :: Piece.SHA1
  } deriving (Show, Eq)

$(makeLenses ''PartialHandshake)

instance Serialize PartialHandshake where
  encode ph = toStrict . BB.toLazyByteString $ mconcat
    [ BB.int32BE . fromIntegral . length $ ph ^. pstrPartial
    , BB.string8 $ ph ^. pstrPartial
    , BB.int64BE 0
    , BB.byteString $ Piece.getSHA1 $ ph ^. hsInfoHashPartial
    ]

data Handshake = Handshake
  { _pstr         :: String
  , _hsInfoHash   :: Piece.SHA1
  , _hsPeerID     :: String
  } deriving (Show, Eq)

$(makeLenses ''Handshake)

instance Serialize Handshake where
  encode h = toStrict $ BB.toLazyByteString $ mconcat
    [ BB.int32BE $ fromIntegral $ length $ h ^. pstr
    , BB.string8 $ h ^. pstr
    , BB.int64BE 0
    , BB.byteString $ Piece.getSHA1 $ h ^. hsInfoHash
    , BB.string8 $ h ^. hsPeerID
    ]

handshakeFromPartial :: PartialHandshake -> String -> Handshake
handshakeFromPartial ph _hsPeerID = Handshake{ _pstr = ph ^. pstrPartial, _hsInfoHash = ph ^. hsInfoHashPartial, ..}

parseHandshakePartOne :: AP.Parser PartialHandshake
parseHandshakePartOne = do
  _pstrPartial       <- int8Parser >>= Bencode.stringParser
  _hsInfoHashPartial <- Piece.SHA1 <$> ( AP.take 8 >> AP.take 20 )
  return PartialHandshake{..}
  where
    int8Parser :: AP.Parser Int
    int8Parser = AP.take 1 >>= return . read . BS8.unpack

parseHandshakePartTwo :: PartialHandshake -> AP.Parser Handshake
parseHandshakePartTwo ph = Bencode.stringParser 20  >>= return . handshakeFromPartial ph

data Msg
  = KeepAliveMsg
  | ChokeMsg
  | UnchokeMsg
  | InterestedMsg
  | UninterestedMsg
  | HaveMsg             Piece.Index
  | BitfieldMsg         B.ByteString
  | RequestMsg          Piece.Index Piece.BIndex Piece.BLength 
  | PieceMsg            Piece.Block
  | CancelMsg           Piece.Index Piece.BIndex Piece.BLength
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

    -- how to represent bitfield
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
      _pIndex  <- parseLength
      _bIndex  <- parseLength
      _bData <- AP.take ( _bLength - 9 )
      return $ PieceMsg Piece.Block{..}

    parseCancel :: AP.Parser Msg
    parseCancel = do
      void $ parseFixedLength 13 >> AP.word8 8
      [i, b, l] <- sequence $ replicate 3 parseLength
      return $ CancelMsg i b l

    parsePort :: AP.Parser Msg
    parsePort = parseFixedLength 3 >> AP.word8 9 >> AP.take 2 >>= ( return . PortMsg . read . BS8.unpack )

peerChoked, clientChoked  :: Handle -> IO ()
peerChoked h = modifyIORef ( h ^. hPeerState ) ( choking .~ True )
clientChoked h = modifyIORef ( h ^. hClientState ) ( choking .~ True )

peerInterested, clientInterested  :: Handle -> IO ()
peerInterested h = modifyIORef ( h ^. hClientState ) ( choking .~ True )
clientInterested h = modifyIORef ( h ^. hClientState ) ( choking .~ True )
