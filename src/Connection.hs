-- | 
{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

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
-- Layer 2: ReaderT (TCP.Sock, TCP.Addr)

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

data Data a = Data
  { _pState   :: IORef ConnState
  , _cState   :: IORef ConnState
  , _other    :: a                      -- infoHash, bitfield
  }

$(makeLenses ''Data)

instance Functor ( Data ) where
  fmap f = ( other %~ f )

initData :: MonadIO m => m ( Data () )
initData = liftIO $ Data <$> ( newIORef $ ConnState{ _choking = False, _interested = True } )
                         <*> ( newIORef $ ConnState{ _choking = False, _interested = True } )
                         <*> ( return () )

type TCP a = ReaderT (TCP.Socket, TCP.SockAddr) ( MaybeT IO ) a

-------------

class Serialize a where
  encode  :: a -> B.ByteString
  decode  :: AP.Parser a

  receiveTCP :: (MonadIO m, MonadReader (TCP.Socket, TCP.SockAddr) m) => Length -> m a
  receiveTCP lenPrefix = do
    (sock, _) <- ask
    xMb <- receiveTCPHelper sock
    case xMb of
      Just x -> return x
      Nothing -> throwString "Failed to receive or parse tracker response."
    where
      receiveTCPHelper :: (MonadIO m, Serialize a) => TCP.Socket -> m ( Maybe a )
      receiveTCPHelper sock = runMaybeT $ do
        lenRaw <- MaybeT $ TCP.recv sock lenPrefix
        len    <- MaybeT . return $ AP.parseOnly parseLengthPrefix lenRaw ^? _Right
        hsRaw  <- MaybeT $ TCP.recv sock len
        MaybeT . return $ AP.parseOnly decode ( lenRaw <> hsRaw ) ^? _Right

  runDecode :: B.ByteString -> Maybe a
  runDecode = ( ^? _Right ) . AP.parseOnly decode

----------------
