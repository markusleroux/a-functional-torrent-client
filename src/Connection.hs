-- | 
{-# LANGUAGE TemplateHaskell #-}

module Connection where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class

import Data.IORef
import qualified Data.ByteString as B
import qualified Data.Attoparsec.ByteString as AP

import Lens.Micro.Platform (_Right, (^?), (%~))
import Lens.Micro.TH

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

data Data a = Data
  { _pState   :: IORef ConnState
  , _cState   :: IORef ConnState
  , _other    :: a                      -- infoHash, bitfield
  }

$(makeLenses ''Data)

initData :: MonadIO m => m ( Data () )
initData = liftIO $ Data <$> ( newIORef $ ConnState{ _choking = False, _interested = True } )
                         <*> ( newIORef $ ConnState{ _choking = False, _interested = True } )
                         <*> ( return () )

type TCP a = ReaderT (TCP.Socket, TCP.SockAddr) ( MaybeT IO ) a

-------------

class Serialize a where
  encode  :: a -> B.ByteString
  decode  :: AP.Parser a

  receiveTCP :: Length -> TCP a
  receiveTCP i = ReaderT $ receiveTCPMsg' decode
    where
      receiveTCPMsg' :: AP.Parser a -> (TCP.Socket, TCP.SockAddr) -> MaybeT IO a
      receiveTCPMsg' parser (sock, _) = do
        lenRaw <- MaybeT $ TCP.recv sock i
        len    <- MaybeT . return $ AP.parseOnly parseLengthPrefix lenRaw ^? _Right
        hsRaw  <- MaybeT $ TCP.recv sock len
        MaybeT . return  $ ( AP.parseOnly parser ( lenRaw <> hsRaw ) ) ^? _Right

  runDecode :: B.ByteString -> Maybe a
  runDecode = ( ^? _Right ) . AP.parseOnly decode

----------------
