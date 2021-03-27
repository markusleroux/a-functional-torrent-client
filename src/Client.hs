-- |
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing -fno-warn-unused-matches #-}
{-# LANGUAGE OverloadedStrings, DataKinds, TemplateHaskell #-}

module Client
  ( PortNumber
  , ConnectionState(..), choking, interested
  , Peer(..), peerID, peerIP, peerPort, peerState, clientState
  , NBytes
  , Client(..)
  , Download(..), dlMeta, dlDownloaded, dlUploaded, dlTrackerResponse, dlPieces, dlBitfield
  , parseHandshake, parseMessage
  ) where

import Prelude hiding (length)

import Data.Maybe
import Data.Array.IO (IOArray)
import Data.Array.MArray
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Char8 as BS8

import Control.Monad.IO.Class
import Control.Monad (void)
import Control.Concurrent.Chan
import Control.Concurrent.MVar

import Lens.Micro.GHC (ix, (^.), (^?!), (<&>), (&), (+~))
import Lens.Micro.TH

import qualified Network.Simple.TCP as TCP
import qualified Network.HTTP.Req as Req

import Torrent
import Bencode
import Piece

--------------------

data ConnectionState =
  ConnectionState
  { _choking       :: Bool
  , _interested    :: Bool
  } deriving (Show, Eq)

$(makeLenses ''ConnectionState)

type PortNumber = Int

data Peer =
  Peer
  { _peerID       :: String
  , _peerIP       :: String
  , _peerPort     :: PortNumber
  , _peerState    :: ConnectionState
  , _clientState  :: ConnectionState
  } deriving (Show, Eq)

$(makeLenses ''Peer)

{-
data TrackerResponse =
  TrackerResponse
  { _failure_reason   :: Maybe String
  , _interval         :: Maybe Int
  , _trackerID        :: Maybe String
  , _complete         :: Maybe Int
  , _incomplete       :: Maybe Int
  , _peers            :: Maybe [Peer]
  } deriving (Show, Eq)
  
$(makeLenses ''TrackerResponse)
-}

data TrackerResponse =
  TrackerResponse
  { _interval'         :: Int
  , _trackerID'        :: String
  , _complete'         :: Int
  , _incomplete'       :: Int
  , _peers'            :: [Peer]
  } deriving (Show, Eq)

$(makeLenses ''TrackerResponse)

type NBytes = Int

data Download =
  Download
  { _dlMeta               :: MetaInfo
  , _dlDownloaded         :: NBytes
  , _dlUploaded           :: NBytes
  , _dlTrackerResponse    :: TrackerResponse
  , _dlPieces             :: IOArray PIndex Piece
  , _dlBitfield           :: Bitfield      -- update only after successful write
  , _dlRoot               :: FilePath
  } deriving (Eq)
  
$(makeLenses ''Download)

getHash :: Download -> PIndex -> SHA1
getHash d pIndex = let startIndex = 20 * pIndex
                       endIndex = startIndex + 20 in
  SHA1 $ takeIn startIndex endIndex $ ( getSHA1 $ d ^. dlMeta . info . pieces )
  where
    takeIn :: Int -> Int -> B.ByteString -> B.ByteString
    takeIn start end = B.take end . B.drop start

tryAddBlock :: Chan Piece -> Download -> IO Download
tryAddBlock pchan d = do
  piece <- readChan pchan
  h <- fmap encodeSHA1 $ collectBlocks $ piece ^. pieceBlocks
  if h == getHash d ( piece ^. pieceIndex )
    then do
      void $ swapMVar ( d ^?! dlBitfield . ix ( piece ^. pieceIndex )) True     -- unsafe
      void $ swapMVar ( piece ^. pieceDone ) True
      return $ d & dlDownloaded +~ ( piece ^. pieceSize )
    else clearBitfield ( piece ^. pieceBitfield ) >> return d

savePieces :: Download -> IO ()
savePieces d = do
  x <- getElems ( d ^. dlPieces )
  bs <- fmap B.concat $ sequence [ collectBlocks $ _pieceBlocks p | p <- x ]
  B.writeFile ( d ^. dlRoot ) bs

data Client =
  Client
  { _IP           :: String
  , _port         :: PortNumber
  , _clientID     :: String
  , _downloads    :: HM.HashMap SHA1 Download
  , _root         :: FilePath
  } deriving (Eq)

$(makeLenses ''Client)

trackerRequest :: (Req.MonadHttp m) => Client -> Download -> m Req.BsResponse
trackerRequest c d = Req.req Req.GET url Req.NoReqBody Req.bsResponse options
  where
    url :: Req.Url 'Req.Http
    url = Req.http $ T.pack $ d ^. dlMeta . announce

    options :: Req.Option 'Req.Http
    options = mconcat $
      [ "info_hash="      Req.=: ( showSHA1 $ d ^. dlMeta . infoHash )
      , "peer_id"         Req.=: ( T.pack $ c ^. clientID )
      , "port="           Req.=: ( T.pack $ show $ c ^. port )
      , "uploaded="       Req.=: ( T.pack $ show $ d ^. dlUploaded )
      , "downloaded="     Req.=: ( T.pack $ show $ d ^. dlDownloaded )
      , "left="           Req.=: ( T.pack $ show $ d ^. dlMeta . info . length - d ^. dlDownloaded )
      , "compact="        Req.=: ( T.pack "0" )
      , "no_peer_id="     Req.=: ( T.pack "0" )
      ]
      
{-
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
-}

parseTrackerResponse :: AP.Parser TrackerResponse
parseTrackerResponse = do
  d <- dictionaryParser
  Just interval       <- return $ intMb     $ d HM.!? "interval"
  Just trackerID      <- return $ stringMb  $ d HM.!? "tracker_id"
  Just complete       <- return $ intMb     $ d HM.!? "complete"
  Just incomplete     <- return $ intMb     $ d HM.!? "incomplete"
  Just peers          <- return $ getPeers  $ d HM.!? "peers"
  return $ TrackerResponse interval trackerID complete incomplete peers
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
  = KeepAliveMessage
  | ChokeMessage
  | UnchokeMessage
  | InterestedMessage
  | UninterestedMessage
  | HaveMessage PIndex
  | BitfieldMessage B.ByteString
  | RequestMessage PIndex BIndex BLength 
  | PieceMessage PIndex BIndex Block
  | CancelMessage PIndex BIndex BLength
  | PortMessage PortNumber
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
    parseKeepAlive = parseFixedLength 0 >> return KeepAliveMessage

    parseChoke :: AP.Parser Message
    parseChoke = parseFixedLength 1 >> AP.word8 0 >> return ChokeMessage

    parseUnchoke :: AP.Parser Message
    parseUnchoke = parseFixedLength 1 >> AP.word8 1 >> return UnchokeMessage

    parseInterested :: AP.Parser Message
    parseInterested = parseFixedLength 1 >> AP.word8 2 >> return InterestedMessage

    parseUninterested :: AP.Parser Message
    parseUninterested = parseFixedLength 1 >> AP.word8 3 >> return UninterestedMessage

    parseHave :: AP.Parser Message
    parseHave = parseFixedLength 5 >> AP.word8 4 >> parseLength >>= return . HaveMessage

    -- how to represent bitfield
    -- discard incorrectly sized bitfields (everywhere really)
    parseBitfield :: AP.Parser Message
    parseBitfield = do
      x <- parseLength
      void $ AP.word8 5
      AP.take ( x - 1 ) <&> BitfieldMessage

    parseRequest :: AP.Parser Message
    parseRequest = do
      void $ parseFixedLength 13 >> AP.word8 6
      i <- parseLength
      b <- parseLength
      l <- parseLength
      return $ RequestMessage i b l

    parsePiece :: AP.Parser Message
    parsePiece = do
      x  <- parseLength
      void $ AP.word8 7
      i  <- parseLength
      b  <- parseLength
      bl <- AP.take ( x - 9 )
      return $ PieceMessage i b bl

    parseCancel :: AP.Parser Message
    parseCancel = do
      void $ parseFixedLength 13 >> AP.word8 8
      i <- parseLength
      b <- parseLength
      l <- parseLength
      return $ CancelMessage i b l

    parsePort :: AP.Parser Message
    parsePort = parseFixedLength 3 >> AP.word8 9 >> AP.take 2 >>= ( return . PortMessage . read . BS8.unpack )

-----------------

clientServer :: MonadIO m => Client -> ((TCP.Socket, TCP.SockAddr) -> IO ()) -> m a
clientServer client f = TCP.serve TCP.HostAny ( show $ client ^. port ) f
