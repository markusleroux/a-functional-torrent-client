-- |
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing -fno-warn-unused-matches #-}
{-# LANGUAGE OverloadedStrings, DataKinds, TemplateHaskell, RecordWildCards #-}

module Client where

import Prelude hiding (length)

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

import Control.Monad.IO.Class
import Control.Concurrent.MVar

import Lens.Micro.GHC ((^.))
import Lens.Micro.TH

import qualified Network.Simple.TCP as TCP
import qualified Network.HTTP.Req as Req

import qualified Download as Down
import qualified Piece

--------------------

data Config = Config
  { _cIP    :: String
  , _cPort  :: Int
  , _cID    :: String
  , _hRoot  :: FilePath
  } deriving (Eq)

data Handle = Handle
  { _hConfig       :: Config
  , _hDownloads    :: HM.HashMap Piece.SHA1 Down.Handle
  } deriving (Eq)

$(makeLenses ''Config)
$(makeLenses ''Handle)

new :: Config -> Handle
new _hConfig = Handle { _hDownloads = HM.empty, .. }
 
trackerRequest :: Req.MonadHttp m => Handle -> Down.Handle -> IO ( m Req.BsResponse )
trackerRequest client d = do
  down  <- readMVar $ d ^. Down.hDownloaded
  up    <- readMVar $ d ^. Down.hUploaded
  return $ Req.req Req.GET url Req.NoReqBody Req.bsResponse $ options down up
  where
    url :: Req.Url 'Req.Http
    url = Req.http $ T.pack $ d ^. Down.hMeta . Down.mAnnounce

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

clientServer :: MonadIO m => Handle -> ((TCP.Socket, TCP.SockAddr) -> IO ()) -> m a
clientServer client = TCP.serve TCP.HostAny ( show $ client ^. hConfig . cPort )
