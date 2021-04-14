-- |
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing -fno-warn-unused-matches #-}
{-# LANGUAGE OverloadedStrings, DataKinds, TemplateHaskell, RecordWildCards, FlexibleInstances, FlexibleContexts, GADTs, MultiParamTypeClasses #-}

module Client where

import Prelude hiding (length)

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.Maybe (isJust)
import Data.Array
import Data.IORef
import qualified Data.ByteString as B

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Concurrent.MVar

import Lens.Micro.Platform (to, ix, (^.), (.~))
import Lens.Micro.TH

import qualified Network.Simple.TCP as TCP
import qualified Network.HTTP.Req as Req
import qualified Network.HTTP.Client as Http

import UnliftIO.Exception
import UnliftIO.Timeout

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
  , _hDownloads    :: IORef ( HM.HashMap SHA1 Down.Handle )
  } deriving (Eq)

$(makeLenses ''Handle)

new :: MonadIO m => Config -> m Handle
new _hConfig =  liftIO $ Handle _hConfig <$> newIORef HM.empty

class HasDownloads a where
  getDownloadsRef :: a -> IORef ( HM.HashMap SHA1 Down.Handle )

  getDownload :: (MonadIO m, HasInfoHash b) => a -> b -> m ( Maybe Down.Handle )
  getDownload x v = do
    downs <- liftIO . readIORef $ getDownloadsRef x
    return $ downs HM.!? ( getInfoHash v )

  knownInfoHash :: (MonadIO m, HasInfoHash b) => a -> b -> m Bool
  knownInfoHash x v = isJust <$> getDownload x v

instance HasDownloads Handle where
  getDownloadsRef = _hDownloads

instance HasDownloads Conn.TCP where
  getDownloadsRef = undefined

class ( HasDownloads env, HasBitfield env, Conn.HasConnectionState env ) => ConnectionEnv env where
  send    :: (MonadIO m, Conn.Encode b) => b -> env -> m ()
  receive :: (MonadIO m, Conn.Decode b) => env -> m b
  close   :: (MonadIO m) => env -> m Bool
  
instance ConnectionEnv Conn.TCP where
  send    = Conn.sendTCP
  receive = Conn.receiveTCP
  close   = undefined

_initServe :: (MonadIO m, ConnectionEnv env, MonadReader env m) => m ()
_initServe = join . reader $ ( \ e -> do
  hs  <- receive e :: MonadIO m => m Peer.Handshake
  isKnown <- knownInfoHash e hs
  if isKnown
    then ( send ( Peer.createHandshake hs ) e ) >> ( runReaderT $ initBF hs ) e
    else throwString "Peer Info Hash not in downloads."
  )

_initConnect :: (MonadIO m, ConnectionEnv env, MonadReader env m) => Peer.Handle -> m ()
_initConnect peer = join . reader $ ( \ e -> do
  send ( Peer.createHandshake peer ) e
  hs <- receive e :: MonadIO m => m Peer.Handshake
  if getInfoHash hs == getInfoHash peer
    then runReaderT ( initBF hs ) e
    else throwString "Peer did not respond with expected info hash."
  )

_connection :: (MonadIO m, ConnectionEnv env, MonadReader env m) => m ()
_connection = join . reader $ ( \ e -> forever $ interact e )
  where
    interact :: ( MonadIO m, ConnectionEnv env ) => env -> m ()
    interact e = do
      msgMb <- liftIO $ timeout (10^6 * 120) ( receive e :: IO Peer.Msg )
      case msgMb of
        Nothing -> undefined
        Just msg -> case msg of
          Peer.KeepAliveMsg    -> handleKeepAlive
          Peer.ChokeMsg        -> handleChokeMsg
          Peer.UnchokeMsg      -> handleUnchokeMsg
          Peer.InterestedMsg   -> handleInterestedMsg
          Peer.UninterestedMsg -> handleUninterestedMsg
          Peer.HaveMsg pIndex  -> handleHaveMsg pIndex
          Peer.BitfieldMsg bs  -> handleBFMsg bs

          Peer.RequestMsg pIndex bIndex bLength -> undefined
          Peer.CancelMsg  pIndex bIndex bLength  -> undefined
          Peer.PieceMsg block  -> undefined
          Peer.PortMsg port    -> undefined
    
class (MonadIO m, ConnectionEnv env, MonadReader env m) => ConnectionAlgo env m where
  connection :: m ()
  connection = _connection
  
  initServe :: m ()
  initServe = _initServe

  initConnect :: Peer.Handle -> m ()
  initConnect = _initConnect
  
  serve :: Handle -> m ()
  connect :: Handle -> Peer.Handle -> m ()

-------------------
 
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

initBF :: ( MonadIO m, MonadReader env m
          , Base.HasBitfield env, HasDownloads env
          , HasInfoHash a) => a -> m ()
initBF v = do
  env    <- ask
  downMb <- getDownload env v
  case downMb of
    Just down -> liftIO $ writeIORef ( getBF env ) $ down ^. Down.hMeta . Down.mInfo . Down.tDLen . to newBF
    Nothing   -> throwString "Failed to initialize bitfield."

----- Regular -------

handleKeepAlive :: m ()
handleKeepAlive = undefined

handleChokeMsg :: (MonadIO m, ConnectionEnv env, MonadReader env m) => m ()
handleChokeMsg = asks Conn.getPeerState >>= Conn.flipChoked

handleUnchokeMsg :: (MonadIO m, ConnectionEnv env, MonadReader env m) => m ()
handleUnchokeMsg = asks Conn.getPeerState >>= Conn.flipChoked

handleInterestedMsg :: (MonadIO m, ConnectionEnv env, MonadReader env m) => m ()
handleInterestedMsg = asks Conn.getPeerState >>= Conn.flipInterested

handleUninterestedMsg :: (MonadIO m, ConnectionEnv env, MonadReader env m) => m ()
handleUninterestedMsg = asks Conn.getPeerState >>= Conn.flipInterested

handleHaveMsg :: (MonadIO m, ConnectionEnv env, MonadReader env m) => PIndex -> m ()
handleHaveMsg pIndex = do
  bf <- getBF <$> ask
  liftIO $ modifyIORef bf ( ix pIndex .~ True )

handleBFMsg :: (MonadIO m, MonadReader env m, Base.HasBitfield env) => B.ByteString -> m ()
handleBFMsg bfBS = do
  bfIORef <- getBF <$> ask
  liftIO . writeIORef bfIORef . bfFromBS $ bfBS

----- Incoming ------

{-
_serve :: (ConnectionAlgo env m) => Handle -> m ()
_serve client = let clientPort  = client ^. hConfig . cPort . to show in
  do
    (peerState, clientState) <- Conn.initState
    bf <- liftIO . newIORef $ array (0, 0) []  -- empty
    TCP.serve TCP.HostAny clientPort $ Conn.uncurryEnv peerState clientState bf
      $ runReaderT ( initServe >> regularConnection )

----- Outgoing ------

_connect :: (MonadIO m) => Handle -> Peer.Handle -> m ()
_connect client peer =
  let
    pIP = peer ^. Peer.cIP
    pPort = peer ^. Peer.cPort . to show
  in do
    (peerState, clientState) <- Conn.initState
    bf <- liftIO $ newIORef $ array (0, 0) []  -- empty
    liftIO $ TCP.connect pIP pPort $ Conn.uncurryEnv peerState clientState bf
      $ runReaderT ( initConnect peer >> regularConnection )

contactPeers :: MonadIO m => Handle -> SHA1 -> m ()
contactPeers client infoHash = do
  down <- getDownload client infoHash
  maybe ( return () ) contactPeers' $ down
  where
    contactPeers' :: MonadIO m => Down.Handle -> m ()
    contactPeers' down = sequence_ $ _connect client <$> peers
      where
        downLen = down ^. Down.hMeta . Down.mInfo . Down.tDLen
        peers   = down ^. Down.hTrackerResponse . Down.trPeers
        
-}
