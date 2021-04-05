-- |
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing -fno-warn-unused-matches #-}
{-# LANGUAGE OverloadedStrings, DataKinds, TemplateHaskell, RecordWildCards, FlexibleInstances #-}

module Client where

import Prelude hiding (length)

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.IORef

import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Concurrent.MVar
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

import Lens.Micro.GHC (to, (^.))
import Lens.Micro.TH

import qualified Network.Simple.TCP as TCP
import qualified Network.HTTP.Req as Req
import qualified Network.HTTP.Client as Http

import qualified Download as Down
import qualified Peer
import Base

--------------------

data Config = Config
  { _cIP    :: IP
  , _cPort  :: Port
  , _cID    :: ID
  , _hRoot  :: FilePath
  } deriving (Eq)

data Handle = Handle
  { _hConfig       :: Config
  , _hDownloads    :: HM.HashMap SHA1 Down.Handle
  } deriving (Eq)

$(makeLenses ''Config)
$(makeLenses ''Handle)

new :: Config -> Handle
new _hConfig = Handle { _hDownloads = HM.empty, .. }

instance Req.MonadHttp ( MaybeT IO ) where
  handleHttpException = const . MaybeT . return $ Nothing

trBodyReader :: Http.Response Http.BodyReader -> IO Down.TrackerResponse
trBodyReader b = Http.responseBody b >>= return . fromJust . runDecode
 
trackerRequest :: Handle -> Down.Handle -> MaybeT IO Down.TrackerResponse
trackerRequest client d = do
  down  <- liftIO $ d ^. Down.hDownloaded . to readMVar
  up    <- liftIO $ d ^. Down.hUploaded   . to readMVar
  Req.reqBr Req.GET url Req.NoReqBody ( options down up ) trBodyReader
  where
    url :: Req.Url 'Req.Http
    url = Req.http $ d ^. Down.hMeta . Down.mAnnounce . to T.pack 

    options :: Int -> Int -> Req.Option 'Req.Http
    options down up = mconcat $
      [ "info_hash="      Req.=: ( id     $ show $ d ^. Down.hMeta . Down.mInfoHash )
      , "peer_id"         Req.=: ( T.pack $ id   $ client ^. hConfig . cID )
      , "port="           Req.=: ( T.pack $ show $ client ^. hConfig . cPort )
      , "uploaded="       Req.=: ( T.pack $ show $ up )
      , "downloaded="     Req.=: ( T.pack $ show $ down )
      , "left="           Req.=: ( T.pack $ show $ d ^. Down.hMeta . Down.mInfo . Down.tDLen - down )
      , "compact="        Req.=: ( T.pack $ id   $ "0" )
      , "no_peer_id="     Req.=: ( T.pack $ id   $ "0" )
      ]

hsToDownloadMb :: Handle -> Peer.Handshake -> Maybe Down.Handle       -- returns Nothing if hash is not in downloads
hsToDownloadMb client peerHSP1 = ( client ^. hDownloads ) HM.!? ( peerHSP1 ^. Peer.hsInfoHash )

peerToDownloadMb :: Handle -> Peer.Handle -> Maybe Down.Handle
peerToDownloadMb client peer = ( client ^. hDownloads ) HM.!? ( peer ^. Peer.hConfig . Peer.cInfoHash )

data InitialData = InitialData
  { _pStateInit  :: IORef ConnectionState
  , _cStateInit  :: IORef ConnectionState
  , _down        :: Down.Handle
  }

$(makeLenses ''InitialData)

getInitialDataFromTorrent :: Down.Handle -> MaybeT IO InitialData
getInitialDataFromTorrent _down = do
  [_pStateInit, _cStateInit] <- liftIO $ sequence . replicate 2 $ newIORef $ ConnectionState{ _choking = False, _interested = True }
  return InitialData{..}

getInitialDataFromHS :: Handle -> MaybeT IO Peer.Handshake -> MaybeT IO InitialData
getInitialDataFromHS client hsMbT = do
  hs <- hsMbT
  _down <- MaybeT . return $ hsToDownloadMb client hs
  getInitialDataFromTorrent _down

-- returns IO Nothing if no hs fails or download is not in downloads list
receiveHS :: Handle -> ReaderT ( TCP.Socket, TCP.SockAddr ) ( MaybeT IO ) InitialData
receiveHS client = mapReaderT ( getInitialDataFromHS client ) $ ReaderT Peer._receiveHSPart1

sendHS :: InitialData -> ReaderT ( TCP.Socket, TCP.SockAddr ) ( MaybeT IO ) InitialData
sendHS iData@(InitialData{ .. }) = ReaderT $ \ (sock, addr) -> do
    TCP.send sock . encode $
      Peer.Handshake { _hsID       = Nothing
                     , _hsPstr     = "BitTorrent Protocol"
                     , _hsInfoHash = _down ^. Down.hMeta . Down.mInfoHash
                     }
    return iData

initConnectionData :: MaybeT IO InitialData -> MaybeT IO ConnectionData
initConnectionData = fmap initConnectionData'
  where
    initConnectionData' :: InitialData -> ConnectionData
    initConnectionData' iData@(InitialData{ _pStateInit = _pState, _cStateInit = _cState, .. }) =
      let dLen = _down ^. Down.hMeta . Down.mInfo . Down.tDLen
      in ConnectionData{ _bitfield = newBF dLen, .. }

initConnectionDataReader ::  ReaderT ( TCP.Socket, TCP.SockAddr ) ( MaybeT IO ) InitialData
  ->  ReaderT ( TCP.Socket, TCP.SockAddr ) ( MaybeT IO ) ConnectionData
initConnectionDataReader = mapReaderT initConnectionData

serve :: ConnectionData -> ReaderT ( TCP.Socket, TCP.SockAddr ) ( MaybeT IO ) ()
serve = undefined

----- Incoming ------

initServer :: Handle -> IO ()
initServer client = let clientPort = client ^. hConfig . cPort . to show
  in
    TCP.serve TCP.HostAny clientPort
      $ void . runMaybeT . runReaderT ( ( receiveHS client >>= sendHS ) ^. to initConnectionDataReader >>= serve )

----- Outgoing ------

connect :: Handle -> Peer.Handle -> IO ()
connect client peer =
  let
    pIP = peer ^. Peer.hConfig . Peer.cIP
    pPort = peer ^. Peer.hConfig . Peer.cPort . to show
       -- _down <- MaybeT . return $ peerToDownloadMb client peer
  in
    TCP.connect pIP pPort $ void . runMaybeT . runReaderT connection
  where
    down = undefined
    
    initFromTorrentWrapped :: Handle -> Down.Handle -> ReaderT ( TCP.Socket, TCP.SockAddr ) ( MaybeT IO ) InitialData
    initFromTorrentWrapped client down = ReaderT . const $ getInitialDataFromTorrent down

    receiveHSWrapped = initConnectionDataReader $ ( receiveHS client ) 

    -- improve receiveHSWrapped to avoid throwing away and recreating
    -- and to get _down from monad
    connection :: ReaderT ( TCP.Socket, TCP.SockAddr ) ( MaybeT IO ) ()
    connection = initFromTorrentWrapped client down >>= sendHS >> receiveHSWrapped >>= serve >> return ()


contactPeers :: Handle -> SHA1 -> IO ()
contactPeers client infoHash = let downMb = ( client ^. hDownloads ) HM.!? infoHash
  in maybe ( return () ) contactPeers' downMb
  where
    contactPeers' :: Down.Handle -> IO ()
    contactPeers' down =
      let
        downLen = down ^. Down.hMeta . Down.mInfo . Down.tDLen
        peersEi   = down ^. Down.hTrackerResponse . Down.trPeers
      in do
        peers <- Down.finishPeersList downLen peersEi
        sequence_ $ connect client <$> peers
