-- |
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing -fno-warn-unused-matches #-}
{-# LANGUAGE OverloadedStrings, DataKinds, TemplateHaskell #-}

module Client where

import Prelude hiding (length)

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

import Control.Monad.IO.Class

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
new c = Handle { _hConfig = c, _hDownloads = HM.empty }

trackerRequest :: (Req.MonadHttp m) => Handle -> Down.Handle -> m Req.BsResponse
trackerRequest client d = Req.req Req.GET url Req.NoReqBody Req.bsResponse options
  where
    url :: Req.Url 'Req.Http
    url = Req.http $ T.pack $ d ^. Down.hMeta . Down.announce

    options :: Req.Option 'Req.Http
    options = mconcat $
      [ "info_hash="      Req.=: ( show   $ d ^. Down.hMeta . Down.infoHash )
      , "peer_id"         Req.=: ( T.pack $ client ^. hConfig . cID )
      , "port="           Req.=: ( T.pack $ show $ client ^. hConfig . cPort )
      , "uploaded="       Req.=: ( T.pack $ show $ d ^. Down.hUploaded )
      , "downloaded="     Req.=: ( T.pack $ show $ d ^. Down.hDownloaded )
      , "left="           Req.=: ( T.pack $ show $ d ^. Down.hMeta . Down.info . Down.length - d ^. Down.hDownloaded )
      , "compact="        Req.=: ( T.pack "0" )
      , "no_peer_id="     Req.=: ( T.pack "0" )
      ]
 
clientServer :: MonadIO m => Handle -> ((TCP.Socket, TCP.SockAddr) -> IO ()) -> m a
clientServer client = TCP.serve TCP.HostAny ( show $ client ^. hConfig . cPort )
