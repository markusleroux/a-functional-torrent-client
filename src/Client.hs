-- |
{-# LANGUAGE OverloadedStrings, DataKinds, TemplateHaskell #-}

module Client where

import Prelude hiding (id, length)

import Data.Word (Word8)
import Data.Int (Int64)
import Data.Maybe
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Char8 as BS8

import Control.Monad.IO.Class

import Lens.Micro.GHC ((^.), (<&>))
import Lens.Micro.TH

import qualified Network.Simple.TCP as TCP
import qualified Network.HTTP.Req as Req

import Torrent
import Bencode

type Index              = Int64
type Begin              = Int64
type PortNumber         = Int64

data ConnectionState =
  ConnectionState
  { _choking       :: Bool
  , _interested    :: Bool
  } deriving (Show, Eq)

-- placeholder
type Block        = B.ByteString

data Peer =
  Peer
  { _peerID       :: String
  , _peerIP       :: String
  , _peerPort     :: PortNumber
  , _peer         :: ConnectionState
  , _clientState  :: ConnectionState
  } deriving (Show, Eq)

$(makeLenses ''Peer)

data TrackerResponse =
  TrackerResponse
  { _failure_reason   :: Maybe String
  , _interval         :: Maybe Int64
  , _trackerID        :: Maybe String
  , _complete         :: Maybe Int64
  , _incomplete       :: Maybe Int64
  , _peers            :: Maybe [Peer]
  } deriving (Show, Eq)
  
$(makeLenses ''TrackerResponse)

data Download =
  Download
  { _meta               :: MetaInfo
  , _downloaded         :: Length
  , _uploaded           :: Length
  , _trackerResponse    :: TrackerResponse
  } deriving (Show, Eq)
  
$(makeLenses ''Download)

data Client =
  Client
  { _IP           :: String
  , _port         :: PortNumber
  , _clientID     :: String
  , _downloads    :: HM.HashMap SHA1 Download
  , _root         :: String
  } deriving (Show, Eq)

$(makeLenses ''Client)

trackerRequest :: (Req.MonadHttp m) => Client -> Download -> m Req.BsResponse
trackerRequest c d = Req.req Req.GET url Req.NoReqBody Req.bsResponse options
  where
    url :: Req.Url 'Req.Http
    url = Req.http $ T.pack $ d ^. meta . announce

    options :: Req.Option 'Req.Http
    options = mconcat $
      [ "info_hash="      Req.=: ( showSHA1 $ d ^. meta . infoHash )
      , "peer_id"         Req.=: ( T.pack $ c ^. clientID )
      , "port="           Req.=: ( T.pack $ show $ c ^. port )
      , "uploaded="       Req.=: ( T.pack $ show $ d ^. uploaded )
      , "downloaded="     Req.=: ( T.pack $ show $ d ^. downloaded )
      , "left="           Req.=: ( T.pack $ show $ d ^. meta . info . length - d ^. downloaded )
      , "compact="        Req.=: ( T.pack "0" )
      , "no_peer_id="     Req.=: ( T.pack "0" )
      ]

parseTrackerResponse :: HM.HashMap B.ByteString BenValue -> TrackerResponse
parseTrackerResponse hm =
  TrackerResponse
    { _failure_reason   = stringMb    $ hm HM.!? "failure_reason"
    , _interval         = intMb       $ hm HM.!? "interval"
    , _trackerID        = stringMb    $ hm HM.!? "tracker_id"
    , _complete         = intMb       $ hm HM.!? "complete"
    , _incomplete       = intMb       $ hm HM.!? "incomplete"
    , _peers            = getPeers    $ hm HM.!? "peers"
    }
  where
    getPeer :: BenValue -> Maybe Peer
    getPeer bv | Just d     <- dictionaryMb $ Just bv
               , Just pID   <- stringMb     $ d HM.!? "peer_id"
               , Just pIP   <- stringMb     $ d HM.!? "ip"
               , Just pPort <- intMb        $ d HM.!? "port"
               = Just $ Peer pIP pID pPort ( ConnectionState True False ) ( ConnectionState True False )
               | otherwise = Nothing

    getPeers :: Maybe BenValue -> Maybe [ Peer ]
    getPeers = fmap ( catMaybes . map getPeer ) . listMb

data TrackerResponse' =
  TrackerResponse'
  { _interval'         :: Int64
  , _trackerID'        :: String
  , _complete'         :: Int64
  , _incomplete'       :: Int64
  , _peers'            :: [Peer]
  } deriving (Show, Eq)

$(makeLenses ''TrackerResponse')

parseTrackerResponse' :: AP.Parser TrackerResponse'
parseTrackerResponse' = do
  d <- dictionaryParser
  Just interval       <- return $ intMb     $ d HM.!? "interval"
  Just trackerID      <- return $ stringMb  $ d HM.!? "tracker_id"
  Just complete       <- return $ intMb     $ d HM.!? "complete"
  Just incomplete     <- return $ intMb     $ d HM.!? "incomplete"
  Just peers          <- return $ getPeers  $ d HM.!? "peers"
  return $ TrackerResponse' interval trackerID complete incomplete peers
  where
    getPeer :: BenValue -> Maybe Peer
    getPeer bv = do
      Just d     <- return $ dictionaryMb $ Just bv
      Just pID   <- return $ stringMb     $ d HM.!? "peer_id"
      Just pIP   <- return $ stringMb     $ d HM.!? "ip"
      Just pPort <- return $ intMb        $ d HM.!? "port"
      Just $ Peer pIP pID pPort ( ConnectionState True False ) ( ConnectionState True False )

    getPeers :: Maybe BenValue -> Maybe [ Peer ]
    getPeers = fmap ( catMaybes . map getPeer ) . listMb

data HandShake =
  Handshake
  { _pstr :: String
  , _hsInfoHash :: SHA1
  , _hsPeerID :: String
  } deriving (Show, Eq)

parseHandshake :: AP.Parser HandShake
parseHandshake = do
  ps <- int8Parser >>= stringParser
  ih <- AP.take 8 >> AP.take 20
  pid <- stringParser 20
  return $ Handshake ps ( SHA1 ih ) pid
  where
    int8Parser :: AP.Parser Int
    int8Parser = AP.take 1 >>= ( return . read . BS8.unpack )

    stringParser :: Int -> AP.Parser String
    stringParser n = AP.take n >>= ( return . BS8.unpack )
 
data Message
  = KeepAlive
  | Choke
  | Unchoke
  | Interested
  | Uninterested
  | Have Index
  | Bitfield [Word8]
  | Request Index Begin Length 
  | Piece Index Begin Block
  | Cancel Index Begin Length
  | Port PortNumber
  deriving (Show, Eq)

parseMessage :: AP.Parser Message
parseMessage = AP.choice
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
    parseFixedLength n = parseLength >>= ( \ l -> if n == l then fail "" else return l )

    parseKeepAlive :: AP.Parser Message
    parseKeepAlive = parseFixedLength 0 >> return KeepAlive

    parseChoke :: AP.Parser Message
    parseChoke = parseFixedLength 1 >> AP.word8 0 >> return Choke

    parseUnchoke :: AP.Parser Message
    parseUnchoke = parseFixedLength 1 >> AP.word8 1 >> return Unchoke

    parseInterested :: AP.Parser Message
    parseInterested = parseFixedLength 1 >> AP.word8 2 >> return Interested

    parseUninterested :: AP.Parser Message
    parseUninterested = parseFixedLength 1 >> AP.word8 3 >> return Uninterested

    parseHave :: AP.Parser Message
    parseHave = parseFixedLength 5 >> AP.word8 4 >> parseLength >>= return . Have . fromIntegral

    -- how to represent bitfield
    -- discard incorrectly sized bitfields (everywhere really)
    parseBitfield :: AP.Parser Message
    parseBitfield = do
      x <- parseLength
      AP.word8 5 >> AP.take ( x - 1 ) <&> Bitfield . B.unpack

    parseRequest :: AP.Parser Message
    parseRequest = do
      i <- parseFixedLength 13 >> AP.word8 6 >> parseLength <&> fromIntegral 
      b <- parseLength <&> fromIntegral 
      l <- parseLength <&> fromIntegral 
      return $ Request i b l

    parsePiece :: AP.Parser Message
    parsePiece = do
      x  <- parseLength
      i  <- AP.word8 7 >> parseLength <&> fromIntegral 
      b  <- parseLength <&> fromIntegral 
      bl <- AP.take ( x - 9 )
      return $ Piece i b bl

    parseCancel :: AP.Parser Message
    parseCancel = do
      i <- parseFixedLength 13 >> AP.word8 8 >> parseLength <&> fromIntegral 
      b <- parseLength <&> fromIntegral 
      l <- parseLength <&> fromIntegral 
      return $ Cancel i b l

    parsePort :: AP.Parser Message
    parsePort = parseFixedLength 3 >> AP.word8 9 >> AP.take 2 >>= ( return . Port . read . BS8.unpack )

-----------------

clientServer :: MonadIO m => Client -> ((TCP.Socket, TCP.SockAddr) -> IO ()) -> m a
clientServer client f = TCP.serve TCP.HostAny ( show $ client ^. port ) f
