-- | 
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing -fno-warn-unused-matches #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards #-}

module Download where

import Prelude hiding (id, length)

import Data.IORef
import Data.Array.IO (IOArray, getElems)
import qualified Data.Array.MArray as MA
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HM
import qualified Data.Attoparsec.ByteString as AP

import Control.Concurrent.Chan
import Control.Concurrent.MVar

import Lens.Micro.TH
import Lens.Micro.GHC (ix, (^.), (^?!), (&), (?~))

import qualified Piece
import qualified Peer
import qualified Bencode

---------------

data MetaInfo = Meta
  { _announce     :: String            -- the URL of the tracker
  , _infoHash     :: Piece.SHA1              -- a hash of info dictionary
  , _info         :: TorrentInfo       -- a dictionary describing the torrent
  } deriving (Show, Eq)

data TorrentInfo = TorrentInfo
  { _name            :: Maybe String    -- the suggested name for the file/directory
  , _piecesLength    :: Piece.Length         -- the number of bytes in each piece of the torrent
  , _pieces          :: Piece.SHA1            -- SHA1 hashes of the pieces
  , _length          :: Int         -- length of the file in bytes (could also be files key for multi-file torrent)
  } deriving (Show, Eq)

$(makeLenses ''MetaInfo)
$(makeLenses ''TorrentInfo)

promptName :: TorrentInfo -> IO TorrentInfo
promptName t = do
  nm <- putStrLn "Please name the torrent." >> getLine
  return $ t & name ?~ nm

getHash :: MetaInfo -> Piece.Index -> Piece.SHA1
getHash meta pIndex = let startIndex = 20 * pIndex
                          endIndex = startIndex + 20 in
  Piece.SHA1 $ takeIn startIndex endIndex $ ( Piece.getSHA1 $ meta ^. info . pieces )
  where
    takeIn :: Int -> Int -> B.ByteString -> B.ByteString
    takeIn start end = B.take end . B.drop start

data TrackerResponsePure = TrackerResponsePure
  { intervalPure   :: Int
  , trackerIDPure  :: String
  , peersConfig    :: [Peer.Config]
  , comp           :: Int
  , incomp         :: Int
  } deriving (Eq)

data TrackerResponse = TrackerResponse
  { _interval         :: Int
  , _trackerID        :: String
  , _complete         :: Int
  , _incomplete       :: Int
  , _peers            :: [Peer.Handle]
  } deriving (Eq)

$(makeLenses ''TrackerResponse)

newTrackerResponseFromPure :: TrackerResponsePure -> IO TrackerResponse
newTrackerResponseFromPure trp = do
  ps <- sequence $ map Peer.newFromConfig $ peersConfig trp
  return TrackerResponse
    { _interval = intervalPure trp
    , _trackerID = trackerIDPure trp
    , _complete = comp trp
    , _incomplete = incomp trp
    , _peers = ps
    }

parseTrackerResponse :: AP.Parser TrackerResponsePure
parseTrackerResponse = do
  d <- Bencode.dictionaryParser
  case do
    interval    <- d HM.!? "interval"    >>= Bencode.intMb
    tid         <- d HM.!? "tracker_id"  >>= Bencode.stringMb
    complete    <- d HM.!? "complete"    >>= Bencode.intMb
    incomplete  <- d HM.!? "incomplete"  >>= Bencode.intMb
    ps          <- d HM.!? "peers"       >>= getPeers
    return (interval, tid, complete, incomplete, ps)
    of Just (intervalPure, trackerIDPure, comp, incomp, peersConfig) -> return TrackerResponsePure{..}
       Nothing -> fail "Failed to find key in tracker response dictionary."
  where
    getPeers :: Bencode.BenValue -> Maybe [Peer.Config]
    getPeers bv = Bencode.listMb bv >>= ( sequence . map getPeer )

    getPeer :: Bencode.BenValue -> Maybe Peer.Config
    getPeer bv = do
      d     <- Bencode.dictionaryMb bv
      pID   <- d HM.!? "peer_id" >>= Bencode.stringMb
      pIP   <- d HM.!? "ip"      >>= Bencode.stringMb
      pPort <- d HM.!? "port"    >>= Bencode.intMb
      return $ Peer.Config pIP pPort pID

data Handle = Handle
  { _hMeta               :: MetaInfo
  , _hDownloaded         :: IORef Int
  , _hUploaded           :: IORef Int
  , _hTrackerResponse    :: TrackerResponse
  , _hPieces             :: IOArray Piece.Index Piece.Handle
  , _hChannel            :: Chan Piece.Handle
  , _hBitfield           :: Piece.Bitfield      -- update only after successful write
  , _hRoot               :: FilePath
  } deriving (Eq)
  
$(makeLenses ''Handle)

new :: MetaInfo -> TrackerResponse -> FilePath -> IO Handle
new _hMeta _hTrackerResponse _hRoot = let l = ceilDiv ( _hMeta ^. info . length ) ( _hMeta ^. info . piecesLength ) in
  do _hPieces     <- sequence [ Piece.new i ( _hMeta ^. info . piecesLength ) () ( getHash _hMeta i ) | i <- [0..l] ]
                       >>= MA.newListArray (0, l)
     _hBitfield   <- Piece.newBitfield l          
     _hDownloaded <- newIORef 0
     _hUploaded   <- newIORef 0
     _hChannel    <- newChan
     return Handle{..}
  where
    ceilDiv :: Int -> Int -> Int
    ceilDiv a b = -div ( -a ) b

getPath :: Handle -> Maybe FilePath
getPath h = (++) ( h ^. hRoot ) <$> h ^. hMeta . info . name

tryAddBlock :: Handle -> IO ()
tryAddBlock d = do
  piece <- readChan $ d ^. hChannel
  h     <- Piece.newSHA1 <$> Piece.collectBlocks piece
  if h == getHash ( d ^. hMeta ) ( piece ^. Piece.hIndex )
    then do
      sequence_ $ map ( flip swapMVar True )
        $ [d ^?! hBitfield . ix ( piece ^. Piece.hIndex ), piece ^. Piece.hDone]     -- unsafe
      modifyIORef ( d ^. hDownloaded ) ( piece ^. Piece.hSize + )
    else Piece.clearBitfield ( piece ^. Piece.hBitfield )

savePieces :: Handle -> IO ()
savePieces d = do
  x  <- getElems ( d ^. hPieces )
  bs <- B.concat <$> sequence [ Piece.collectBlocks p | p <- x ]
  B.writeFile ( d ^. hRoot ) bs
