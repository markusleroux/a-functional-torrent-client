-- |
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing -fno-warn-unused-matches #-}
{-# LANGUAGE OverloadedStrings, DataKinds, TemplateHaskell, RecordWildCards, FlexibleInstances, FlexibleContexts #-}

module Client where

import Prelude hiding (length)

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.IORef

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Concurrent.MVar

import Lens.Micro.Platform (to, ix, (^.), (^?), (.~), (&), (<&>))
import Lens.Micro.TH

import qualified Network.Simple.TCP as TCP
import qualified Network.HTTP.Req as Req
import qualified Network.HTTP.Client as Http

import UnliftIO.Exception

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
 
trackerRequest :: (MonadIO m, Req.MonadHttp m) => Handle -> Down.Handle -> m Down.TrackerResponse
trackerRequest client d = do
  down  <- liftIO $ d ^. Down.hDownloaded . to readMVar
  up    <- liftIO $ d ^. Down.hUploaded   . to readMVar
  trMb  <- Req.reqBr Req.GET url Req.NoReqBody ( options down up ) trBodyReader
  case trMb of
    Just tr -> return tr
    Nothing -> throwString "Failed to get tracker response."
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
receive :: (MonadIO m, MonadReader (TCP.Socket, TCP.SockAddr) m, Conn.Serialize a)
  => Handle -> Length -> Conn.Data b -> m ( Conn.Data a )
receive client lenPrefix ( Conn.Data{..} ) = do
  _other <- Conn.receiveTCP lenPrefix
  return Conn.Data{ .. }

-- returns IO Nothing if hs fails or download is not in downloads list
receiveHS :: (MonadIO m, MonadReader (TCP.Socket, TCP.SockAddr) m)
  => Handle -> Conn.Data a -> m ( Conn.Data Peer.Handshake )
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

send :: (MonadIO m, MonadReader (TCP.Socket, TCP.SockAddr) m, Conn.Serialize a)
     => Handle -> a -> m a 
send client x = do
  (sock, _) <- ask
  TCP.send sock $ Conn.encode x
  return x

sendAs :: (MonadIO m, MonadReader (TCP.Socket, TCP.SockAddr) m, Conn.Serialize b)
       => Handle -> ( a -> b ) -> a -> m a
sendAs client mutate x = send client ( mutate x ) >> return x

sendHS :: (MonadIO m, MonadReader (TCP.Socket, TCP.SockAddr) m)
       => Handle -> Conn.Data Peer.Handshake -> m ( Conn.Data Peer.Handshake )
sendHS = flip sendAs ( ^. Conn.other )

initPeerBF :: (MonadIO m, MonadReader (TCP.Socket, TCP.SockAddr) m)
             => Handle -> Conn.Data Peer.Handshake -> m ( Conn.Data Bitfield )
initPeerBF client iData@( Conn.Data{ .. } ) =
  case hsToDownloadMb client _other of
    Just _down -> return Conn.Data{ _other = newBF $ _down ^. Down.hMeta . Down.mInfo . Down.tDLen, .. }
    Nothing -> throwString "Failed to initialize bitfield."

-- initPeerBF' :: Handle -> Conn.Data Peer.Handshake -> Conn.TCP ( Conn.Data Bitfield )
-- initPeerBF' client iData@( Conn.Data{ .. } ) =
--   ReaderT $ \ ( sock, addr ) -> MaybeT . return $ do
--     _down <- hsToDownloadMb client _other
--     return Conn.Data{ _other = newBF $ _down ^. Down.hMeta . Down.mInfo . Down.tDLen, .. }

-- initPeerBF :: Handle -> Conn.Data Peer.Handshake -> Conn.TCP ( Conn.Data Bitfield )
-- initPeerBF client iData@( Conn.Data{ .. } ) =
--   ReaderT $ \ ( sock, addr ) -> MaybeT . return $ do
--     _down <- hsToDownloadMb client _other
--     return Conn.Data{ _other = newBF $ _down ^. Down.hMeta . Down.mInfo . Down.tDLen, .. }

regularConnection :: Conn.Data Bitfield -> Conn.TCP ()
regularConnection Conn.Data { .. } = undefined

----- Incoming ------

compareInfoHash :: (MonadIO m) => Handle -> Conn.Data Peer.Handshake -> m ( Conn.Data Peer.Handshake )
compareInfoHash client Conn.Data{..} =
  case getHS client _other of
    Just hs -> return Conn.Data{ _other = hs, .. }
    Nothing -> throwString "Peer info hash not found locally."

initServer :: (MonadIO m, MonadReader (TCP.Socket, TCP.SockAddr) m) => Handle -> m ( Conn.Data Bitfield )
initServer client = Conn.initData >>= receiveHS' >>= compareInfoHash' >>= sendHS' >>= initPeerBF'
  where
    receiveHS'       = receiveHS         client
    compareInfoHash' = compareInfoHash   client
    sendHS'          = sendHS            client
    initPeerBF'      = initPeerBF      client

serve :: Handle -> IO ()
serve client =
  let
    clientPort  = client ^. hConfig . cPort . to show
    initServer' = initServer client
  in
    TCP.serve TCP.HostAny clientPort
      $ void . runMaybeT . runReaderT ( initServer' >>= regularConnection )

----- Outgoing ------

signHS :: (MonadIO m) => Handle -> Peer.Handle -> Conn.Data () -> m (Conn.Data Peer.Handshake)
signHS client peer Conn.Data{..} = do
  case lookupHS client $ both $ peer ^. Peer.hInfoHash of
    Just myHS -> return Conn.Data{ _other = myHS, .. }
    Nothing -> throwString "Peer info hash not found locally."

initConnect :: (MonadIO m, MonadReader (TCP.Socket, TCP.SockAddr) m) => Handle -> Peer.Handle -> m ( Conn.Data Bitfield )
initConnect client peer = Conn.initData >>= signHS' >>= sendHS' >>= receiveHS' >>= initPeerBF'
  where
    signHS'      = signHS       client peer
    sendHS'      = sendHS       client
    receiveHS'   = receiveHS    client
    initPeerBF'  = initPeerBF client

-- improve receiveHSWrapped to avoid throwing away and recreating
-- and to get _down from monad
connect :: MonadIO m => Handle -> Peer.Handle -> m ()
connect client peer =
  let
    pIP = peer ^. Peer.cIP
    pPort = peer ^. Peer.cPort . to show
  in
    liftIO $ TCP.connect pIP pPort
      $ void . runMaybeT . runReaderT ( initConnect' >>= regularConnection )
  where
    initConnect' = initConnect client peer

contactPeers :: MonadIO m => Handle -> SHA1 -> m ()
contactPeers client infoHash = maybe ( return () ) contactPeers' $ downMb
  where
    downMb = client ^? hDownloads . ix infoHash
    
    contactPeers' :: MonadIO m => Down.Handle -> m ()
    contactPeers' down = sequence_ $ connect client <$> peers
      where
        downLen = down ^. Down.hMeta . Down.mInfo . Down.tDLen
        peers   = down ^. Down.hTrackerResponse . Down.trPeers
        
