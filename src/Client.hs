-- |
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing -fno-warn-unused-matches #-}
{-# LANGUAGE OverloadedStrings, DataKinds, TemplateHaskell, RecordWildCards, FlexibleInstances #-}

module Client where

import Prelude hiding (length)

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.IORef

import Control.Monad (void, join)
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Concurrent.MVar

import Lens.Micro.Platform (to, ix, (^.), (^?), (.~), (&))
import Lens.Micro.TH

import qualified Network.Simple.TCP as TCP
import qualified Network.HTTP.Req as Req
import qualified Network.HTTP.Client as Http

import qualified Download as Down
import qualified Peer
import Base
import qualified Connection as Conn

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
 
trackerRequest :: Handle -> Down.Handle -> MaybeT IO Down.TrackerResponse
trackerRequest client d = do
  down  <- liftIO $ d ^. Down.hDownloaded . to readMVar
  up    <- liftIO $ d ^. Down.hUploaded   . to readMVar
  MaybeT . fmap join . runMaybeT $ Req.reqBr Req.GET url Req.NoReqBody ( options down up ) trBodyReader
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

    trBodyReader :: Http.Response Http.BodyReader -> IO ( Maybe Down.TrackerResponse )
    trBodyReader b = Http.responseBody b >>= return . Conn.runDecode

hsToDownloadMb :: Handle -> Peer.Handshake -> Maybe Down.Handle       -- returns Nothing if hash is not in downloads
hsToDownloadMb client peerHSP1 = client ^? hDownloads . ix ( peerHSP1 ^. Peer.hsInfoHash )

peerToDownloadMb :: Handle -> Peer.Handle -> Maybe Down.Handle
peerToDownloadMb client peer = client ^? hDownloads . ix ( peer ^. Peer.hInfoHash . to both )

-- returns IO Nothing if hs fails or download is not in downloads list
receiveHS :: Handle -> Conn.Data () -> Conn.TCP ( Conn.Data Peer.Handshake )
receiveHS client ( Conn.Data{..} ) = do
  hs <- Peer._receiveHSPart1
  return $ Conn.Data{ _other = hs, .. }

-- create my hs if hash is in downloads
lookupHS :: Handle -> SHA1 -> Maybe Peer.Handshake
lookupHS client hash = do
    _down <- client ^? hDownloads . ix hash
    return  Peer.Handshake { _hsID       = Nothing
                           , _hsPstr     = "BitTorrent Protocol"
                           , _hsInfoHash = _down ^. Down.hMeta . Down.mInfoHash
                           }

-- create my hs from another hs
getHS :: Handle -> Peer.Handshake -> Maybe Peer.Handshake
getHS client hs = join $ lookupHS client <$> hs ^? Peer.hsInfoHash

sendHS :: Handle -> Conn.Data Peer.Handshake -> Conn.TCP ( Conn.Data Peer.Handshake )
sendHS client cData@( Conn.Data{ _other = _hs } ) = do
  (sock, _) <- ask
  TCP.send sock $ Conn.encode _hs
  return cData

initPeerBF :: Handle -> Conn.Data Peer.Handshake -> Conn.TCP ( Conn.Data Bitfield )
initPeerBF client iData@( Conn.Data{ .. } ) =
  ReaderT $ \ ( sock, addr ) -> MaybeT . return $ do
    _down <- hsToDownloadMb client _other
    return Conn.Data{ _other = newBF $ _down ^. Down.hMeta . Down.mInfo . Down.tDLen, .. }

regularConnection :: Conn.Data Bitfield -> Conn.TCP ()
regularConnection Conn.Data { .. } = undefined

----- Incoming ------

-- MyHS -> HS -> Bitfield -> ()

initServer :: Handle -> Conn.TCP ( Conn.Data Bitfield )
initServer client = Conn.initData >>= receiveHS' >>= sendHS' >>= initPeerBF'
  where
    receiveHS'  = receiveHS  client
    initPeerBF' = initPeerBF client
    sendHS'     = sendHS     client

getInitialDataFromTorrent :: MonadIO m => Down.Handle -> m ( Conn.Data SHA1 )
getInitialDataFromTorrent _down = liftIO $
  Conn.Data <$> ( newIORef $ Conn.ConnState{ _choking = False, _interested = True } )
            <*> ( newIORef $ Conn.ConnState{ _choking = False, _interested = True } )
            <*> ( _down ^. Down.hMeta . Down.mInfoHash . to return )

serve :: Handle -> IO ()
serve client =
  let
    clientPort  = client ^. hConfig . cPort . to show
    initServer' = initServer client
  in
    TCP.serve TCP.HostAny clientPort
      $ void . runMaybeT . runReaderT ( initServer' >>= regularConnection )

----- Outgoing ------

deepJoin :: Conn.TCP ( Conn.Data ( Maybe a ) ) -> Conn.TCP ( Conn.Data a )
deepJoin f = do
  Conn.Data{..} <- f
  lift . MaybeT . return $ Conn.Data _pState _cState <$> _other

-- Peer.Config -> MyHS -> HS -> Bitfield -> ()
  
initConnect :: Handle -> Peer.Handle -> Conn.TCP ( Conn.Data Bitfield )
initConnect client peer = let infoHash = both $ peer ^. Peer.hInfoHash in
  do
    init <- Conn.initData
    signedInit <- deepJoin . return $ init & Conn.other .~ ( lookupHS client infoHash )
    void $ sendHS client signedInit

    receiveHS' init >>= initPeerBF'
  where

    receiveHS'   = receiveHS client
    initPeerBF'  = initPeerBF client

-- fail if InfoHash of client does not match to known download
getInitialDataFromHS :: Handle -> Peer.Handshake -> MaybeT IO ( Conn.Data SHA1 )
getInitialDataFromHS client hs = MaybeT . sequence $ getInitialDataFromTorrent <$> hsToDownloadMb client hs

-- improve receiveHSWrapped to avoid throwing away and recreating
-- and to get _down from monad
connect :: Handle -> Peer.Handle -> IO ()
connect client peer =
  TCP.connect pIP pPort
    $ void . runMaybeT . runReaderT ( initConnect' >>= regularConnection )
  where
    initConnect' = initConnect client peer
    
    pIP = peer ^. Peer.cIP
    pPort = peer ^. Peer.cPort . to show


contactPeers :: Handle -> SHA1 -> IO ()
contactPeers client infoHash = let downMb = client ^? hDownloads . ix infoHash
  in maybe ( return () ) contactPeers' downMb
  where
    contactPeers' :: Down.Handle -> IO ()
    contactPeers' down =
      let
        downLen = down ^. Down.hMeta . Down.mInfo . Down.tDLen
        peers   = down ^. Down.hTrackerResponse . Down.trPeers
      in do
        sequence_ $ connect client <$> peers
