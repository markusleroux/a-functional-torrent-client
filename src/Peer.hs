-- | 
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing -fno-warn-unused-matches #-}
{-# LANGUAGE OverloadedStrings, DataKinds, TemplateHaskell, RecordWildCards, FlexibleContexts #-}

module Peer where

import qualified Data.ByteString as B
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Builder as BB
import Data.ByteString.Lazy (toStrict)
import Data.Maybe

import Control.Monad.Reader

import Lens.Micro.TH
import Lens.Micro.Platform (to, (&), (<&>), (^.), (.~), (?~))

import Base
import Connection
import qualified Piece
import qualified Bencode

------- Handle --------

data Handle = Handle
  { _cIP        :: IP
  , _cPort      :: Port
  , _cID        :: Maybe ID
  , _hInfoHash  :: SHA1
  }

$(makeLenses ''Handle)

instance Show Handle where
  show h = concat [ "Peer ", id, "( ", ip, port, " )"]
    where
      id     = h ^. cID . to show
      ip     = h ^. cIP
      port   = h ^. cPort . to show

instance Eq Handle where
  (==) peer other = peer ^. cID == peer ^. cID

instance HasInfoHash Handle where
  getInfoHash = _hInfoHash
  
------ Handshake -------

data Handshake = Handshake
  { _hsPstr       :: String
  , _hsInfoHash   :: SHA1
  , _hsID         :: Maybe ID
  } deriving (Show, Eq)

$(makeLenses ''Handshake)

instance HasInfoHash Handshake where
  getInfoHash = _hsInfoHash

_encodeHS :: Handshake -> B.ByteString
_encodeHS h
    | h ^. hsID . to isJust
    = ( h ^. hsID . to fromJust . to BB.string8 . to BB.toLazyByteString . to toStrict ) <> encode ( h & hsID .~ Nothing )
    | otherwise
    = toStrictBS $ mconcat
      [ h ^. hsPstr . to length . to fromIntegral . to BB.int32BE
      , h ^. hsPstr . to BB.string8
      , BB.int64BE 0
      , h ^. hsInfoHash . to getSHA1 . to BB.byteString
      ]

instance Encode Handshake where
  encode = _encodeHS

_decodeHSPart1 :: AP.Parser Handshake
_decodeHSPart1 = do
  _hsPstr     <- int8Parser >>= Bencode.stringParser
  _hsInfoHash <- SHA1 <$> ( AP.take 8 >> AP.take 20 )
  return Handshake{ _hsID = Nothing, .. }
  where
    int8Parser :: AP.Parser Int
    int8Parser = AP.take 1 >>= return . read . BS8.unpack

_decodeHSPart2 :: Handshake -> AP.Parser Handshake
_decodeHSPart2 ph = Bencode.stringParser 20 >>= return . handshakeFromPartial ph
  where
    handshakeFromPartial :: Handshake -> ID -> Handshake
    handshakeFromPartial = flip ( hsID ?~ )

-- handshake is typically sent in two parts, with ID coming after bitfield message
-- this is probably not to be used
_decodeHS :: AP.Parser Handshake
_decodeHS = _decodeHSPart1 >>= _decodeHSPart2

instance Decode Handshake where
  decode = _decodeHSPart1
      
instance Serialize Handshake

createHandshake :: HasInfoHash a => a -> Peer.Handshake
createHandshake v =
  Peer.Handshake { _hsID       = Nothing
                 , _hsPstr     = "BitTorrent Protocol"
                 , _hsInfoHash = getInfoHash v
                 }

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

_decodeMsg :: AP.Parser Msg
_decodeMsg = AP.choice
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
    parseFixedLength n = do
      lenPre <- parseLengthPrefix
      if n /= lenPre
        then fail "Length does not match expected length."
        else return lenPre

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

instance Decode Msg where
  decode = _decodeMsg

_encodeInt32BE, lenPrefixMsg, idPrefixMsg :: Int -> BB.Builder
_encodeInt32BE  = BB.int32BE . fromIntegral
lenPrefixMsg   = _encodeInt32BE
idPrefixMsg    = BB.int8    . fromIntegral

_encodeMsg :: Msg -> B.ByteString
_encodeMsg KeepAliveMsg = toStrictBS
  $ lenPrefixMsg 0
_encodeMsg ChokeMsg = toStrictBS
  $ lenPrefixMsg 1
  <> idPrefixMsg 0
_encodeMsg UnchokeMsg = toStrictBS
  $ lenPrefixMsg 1
  <> idPrefixMsg 1
_encodeMsg InterestedMsg = toStrictBS
  $ lenPrefixMsg 1
  <> idPrefixMsg 2
_encodeMsg UninterestedMsg = toStrictBS
  $ lenPrefixMsg 1
  <> idPrefixMsg 3
_encodeMsg ( HaveMsg _pIndex ) = toStrictBS
  $ lenPrefixMsg 5
  <> idPrefixMsg 4
  <> _encodeInt32BE _pIndex
_encodeMsg ( BitfieldMsg _bf ) = toStrictBS
  $ lenPrefixMsg ( 1 + B.length _bf )
  <> idPrefixMsg 5
  <> BB.byteString _bf
_encodeMsg ( RequestMsg _pIndex _bIndex _bLen ) = toStrictBS
  $ lenPrefixMsg 13
  <> idPrefixMsg 6
  <> _encodeInt32BE _pIndex
  <> _encodeInt32BE _bIndex
  <> _encodeInt32BE _bLen
_encodeMsg ( PieceMsg _block ) = encode _block
_encodeMsg ( CancelMsg _pIndex _bIndex _bLen ) = toStrictBS
  $ lenPrefixMsg 13
  <> idPrefixMsg 8
  <> _encodeInt32BE _pIndex
  <> _encodeInt32BE _bIndex
  <> _encodeInt32BE _bLen
_encodeMsg ( PortMsg _port ) = toStrictBS
  $ lenPrefixMsg 3
  <> idPrefixMsg 9
  <> ( BB.int16BE . fromIntegral $ _port )

instance Encode Msg where
  encode = _encodeMsg
  
instance Serialize Msg
