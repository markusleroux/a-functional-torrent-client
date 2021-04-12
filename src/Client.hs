-- |
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing -fno-warn-unused-matches #-}
{-# LANGUAGE OverloadedStrings, DataKinds, TemplateHaskell, RecordWildCards, FlexibleInstances, FlexibleContexts #-}

module Client where

import Prelude hiding (length)

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.Maybe (isJust)
import Data.Array
import Data.IORef

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Concurrent.MVar

import Lens.Micro.Platform (to, ix, (^.), (^?))
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

$(makeLenses ''Config)

data Handle = Handle
  { _hConfig       :: Config
  , _hDownloads    :: HM.HashMap SHA1 Down.Handle
  } deriving (Eq)

$(makeLenses ''Handle)

new :: Config -> Handle
new _hConfig = Handle { _hDownloads = HM.empty, .. }

class HasDownloads a where
  getDownloads :: a -> HM.HashMap SHA1 Down.Handle

  getDownload :: HasInfoHash b => a -> b -> Maybe Down.Handle
  getDownload x v = ( getDownloads x ) HM.!? ( getInfoHash v )

  knownInfoHash :: HasInfoHash b => a -> b -> Bool
  knownInfoHash x v = isJust $ getDownload x v

instance HasDownloads Handle where
  getDownloads = _hDownloads

instance HasDownloads Conn.Env where
  getDownloads = undefined
 
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

regularConnection :: m ()
regularConnection = undefined

----- Incoming ------

initServer :: (MonadIO m, MonadReader env m, Conn.HasTCP env, HasDownloads env) => m ()
initServer = do
  env <- ask
  hs  <- Conn.receiveTCP env :: m Peer.Handshake
  if knownInfoHash env hs
    then Conn.sendTCP env $ Peer.createHandshake hs
    else throwString "Peer Info Hash not in downloads."

serve :: Handle -> IO ()
serve client =
  let
    clientPort  = client ^. hConfig . cPort . to show
  in do
    (peerState, clientState) <- Conn.initState
    bf <- newIORef $ array (0, 0) []  -- empty
    TCP.serve TCP.HostAny clientPort $ Conn.uncurryEnv peerState clientState bf
      $ runReaderT ( initServer >> regularConnection )

----- Outgoing ------

initConnect :: (MonadIO m, MonadReader env m, Conn.HasTCP env, HasDownloads env) => Peer.Handle -> m ()
initConnect peer = do
  env <- ask
  Conn.sendTCP env $ Peer.createHandshake peer
  hs <- Conn.receiveTCP env :: m Peer.Handshake
  if getInfoHash hs == getInfoHash peer
    then return ()
    else throwString "Peer did not respond with expected info hash"
  

-- improve receiveHSWrapped to avoid throwing away and recreating
-- and to get _down from monad
connect :: (MonadIO m) => Handle -> Peer.Handle -> m ()
connect client peer =
  let
    pIP = peer ^. Peer.cIP
    pPort = peer ^. Peer.cPort . to show
  in do
    (peerState, clientState) <- Conn.initState
    bf <- liftIO $ newIORef $ array (0, 0) []  -- empty
    liftIO $ TCP.connect pIP pPort $ Conn.uncurryEnv peerState clientState bf
      $ runReaderT ( initConnect peer >> regularConnection )

contactPeers :: MonadIO m => Handle -> SHA1 -> m ()
contactPeers client infoHash = maybe ( return () ) contactPeers' $ downMb
  where
    downMb = client ^? hDownloads . ix infoHash
    
    contactPeers' :: MonadIO m => Down.Handle -> m ()
    contactPeers' down = sequence_ $ connect client <$> peers
      where
        downLen = down ^. Down.hMeta . Down.mInfo . Down.tDLen
        peers   = down ^. Down.hTrackerResponse . Down.trPeers
        
