-- | 
{-# LANGUAGE TemplateHaskell, FlexibleContexts, RecordWildCards, GeneralizedNewtypeDeriving, GADTs #-}

module Connection where

import Control.Monad.Trans.Maybe
import Control.Monad.Reader

import Data.IORef
import qualified Data.ByteString as B
import qualified Data.Attoparsec.ByteString as AP

import Lens.Micro.Platform (_Right, (^?), (%~))
import Lens.Micro.TH

import UnliftIO.Exception

import qualified Network.Simple.TCP as TCP

import Base

---------------

data ConnState = ConnState
  { _choking       :: Bool
  , _interested    :: Bool
  } deriving (Show, Eq)

$(makeLenses ''ConnState)

flipChoked, flipInterested :: MonadIO m => IORef ConnState -> m ()
flipChoked     = liftIO . flip modifyIORef ( choking %~ not )
flipInterested = liftIO . flip modifyIORef ( interested %~ not )

type PeerState = IORef ConnState
type ClientState = IORef ConnState

initState :: MonadIO m => m (PeerState, ClientState)
initState = undefined

data TCP = TCP
  { _eTCP         :: (TCP.Socket, TCP.SockAddr)
  , _ePeerState   :: PeerState
  , _eClientState :: ClientState
  , _eBF          :: IORef Bitfield
  }

$(makeLenses ''TCP)

uncurryEnv :: (MonadIO m) => PeerState -> ClientState -> IORef Bitfield -> ( TCP -> m a ) -> (TCP.Socket, TCP.SockAddr) -> m a
uncurryEnv _ePeerState _eClientState _eBF f _eTCP = f $ TCP{..}

instance HasBitfield TCP where
  getBF = _eBF

----------------

class TCPEnv a where
  getTCP :: a -> (TCP.Socket, TCP.SockAddr)

getSock :: TCPEnv env => env -> TCP.Socket
getSock = fst . getTCP

sendTCPAs :: (Encode c, MonadIO m, TCPEnv env) => b -> ( b -> c ) -> env -> m ()
sendTCPAs v mutate e = do
  TCP.send ( getSock e ) ( encode . mutate $ v )

sendTCP   :: (Encode b, MonadIO m, TCPEnv env) => b -> env -> m ()
sendTCP v e = sendTCPAs v id e

receiveTCP :: (Decode b, MonadIO m, TCPEnv env) => env -> m b
receiveTCP e = do
  vMb <- receiveTCPHelper $ getSock e
  case vMb of
    Just v  -> return v
    Nothing -> throwString "Failed to receive or parse tracker response."
  where
    receiveTCPHelper :: (Decode b, MonadIO m) => TCP.Socket -> m ( Maybe b )
    receiveTCPHelper sock = runMaybeT $ do
      lenRaw <- MaybeT $ TCP.recv sock 4
      len    <- MaybeT . return $ AP.parseOnly parseLengthPrefix lenRaw ^? _Right
      hsRaw  <- MaybeT $ TCP.recv sock len
      MaybeT . return $ AP.parseOnly decode ( lenRaw <> hsRaw ) ^? _Right

instance TCPEnv TCP where
  getTCP = _eTCP

----------------

class HasConnectionState a where
  getPeerState :: a -> IORef ConnState
  getClientState :: a -> IORef ConnState

instance HasConnectionState TCP where
  getPeerState = _ePeerState
  getClientState = _eClientState
  
----------------

class Encode a where
  encode  :: a -> B.ByteString
  
class Decode a where
  decode  :: AP.Parser a

  runDecode :: B.ByteString -> Maybe a
  runDecode = ( ^? _Right ) . AP.parseOnly decode
  
class (Encode a, Decode a) => Serialize a

------------------

