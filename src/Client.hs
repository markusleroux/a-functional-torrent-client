-- |
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing -fno-warn-unused-matches #-}
{-# LANGUAGE OverloadedStrings, DataKinds, TemplateHaskell, RecordWildCards, FlexibleInstances #-}

module Client where

import Prelude hiding (length)

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Either

import Control.Monad (when, join, void)
import Control.Monad.IO.Class
import Control.Concurrent.MVar
import Control.Monad.Trans.Maybe

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
      [ "info_hash="      Req.=: ( show   $ d ^. Down.hMeta . Down.mInfoHash )
      , "peer_id"         Req.=: ( T.pack $ client ^. hConfig . cID )
      , "port="           Req.=: ( T.pack $ show $ client ^. hConfig . cPort )
      , "uploaded="       Req.=: ( T.pack $ show $ up )
      , "downloaded="     Req.=: ( T.pack $ show $ down )
      , "left="           Req.=: ( T.pack $ show $ d ^. Down.hMeta . Down.mInfo . Down.tDLen - down )
      , "compact="        Req.=: ( T.pack "0" )
      , "no_peer_id="     Req.=: ( T.pack "0" )
      ]

verifyHS :: Handle -> Peer.Handshake -> MaybeT IO Down.Handle
verifyHS client peerHSP1 = do
  down     <- MaybeT . return $ ( client ^. hDownloads ) HM.!? ( peerHSP1 ^. Peer.hsInfoHash )
  if ( Down.verifyInfoHash down $ peerHSP1 ^. Peer.hsInfoHash )
    then return down
    else MaybeT $ return Nothing

receiveAndVerifyHS :: Handle -> (TCP.Socket, TCP.SockAddr) -> MaybeT IO Down.Handle
receiveAndVerifyHS h p@(sock, addr) = Peer.receiveHSPartOne p >>= verifyHS h

sendHS :: (TCP.Socket, TCP.SockAddr) -> Down.Handle -> IO ()
sendHS (sock, _) down =
  let hs = Peer.Handshake{ _hsID = Nothing,
                           _hsPstr = "BitTorrent Protocol",
                           _hsInfoHash = down ^. Down.hMeta . Down.mInfoHash }
  in TCP.send sock $ encode hs

serve :: Handle -> (TCP.Socket, TCP.SockAddr) -> IO ()
serve = undefined

clientServer :: Handle -> MaybeT IO a
clientServer client = TCP.serve TCP.HostAny ( show $ client ^. hConfig . cPort ) $ servePeer client
  where
    servePeer :: Handle -> (TCP.Socket, TCP.SockAddr) -> IO ()
    servePeer client p = do
      mb <- fmap isJust . runMaybeT $ peerInitHS client p
      when mb ( serve client p )
        
    peerInitHS :: Handle -> (TCP.Socket, TCP.SockAddr) -> MaybeT IO ()
    peerInitHS h p = receiveAndVerifyHS h p >>= liftIO . sendHS p

selfInitConnection :: (TCP.Socket, TCP.SockAddr) -> ID -> Handle -> Down.Handle -> MaybeT IO ()
selfInitConnection p pID client down = do
  liftIO $ sendHS p down
  hs <- Peer.receiveHSPartOne p
  if Down.hasPeerID ( down ^. Down.hTrackerResponse ) pID
    then liftIO $ serve client p
    else MaybeT . return $ Nothing

contactPeers :: Handle -> SHA1 -> IO ()
contactPeers client iHash = maybe ( return () ) contactPeers' $ ( client ^. hDownloads ) HM.!? iHash
  where
    contactPeers' :: Down.Handle -> IO ()
    contactPeers' down = do
      ps <- Down.finishPeersList ( down ^. Down.hMeta . Down.mInfo . Down.tDLen ) $ down ^. Down.hTrackerResponse . Down.trPeers
      sequence_ $ contactPeer down <$> ps

    contactPeer :: Down.Handle -> Peer.Handle -> IO ()
    contactPeer down h = TCP.connect peerIP peerPort peerConnection
      where
        peerIP = h ^. Peer.hConfig . Peer.hIP
        peerID = h ^. Peer.hConfig . Peer.hID
        peerPort = show $ h ^. Peer.hConfig . Peer.hPort
        
        peerConnection :: (TCP.Socket, TCP.SockAddr) -> IO ()
        peerConnection p = void . runMaybeT $ selfInitConnection p peerID client down
