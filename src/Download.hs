-- | 
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing -fno-warn-unused-matches #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards #-}

module Download where

import Prelude hiding (id, length)

import Data.Tuple.Sequence
import Data.Array.IO (IOArray, getElems)
import qualified Data.Array.MArray as MA
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HM
import qualified Data.Attoparsec.ByteString as AP
import Data.Either (isLeft)

import Control.Monad
import Control.Concurrent.Chan
import Control.Concurrent.MVar

import Lens.Micro.TH
import Lens.Micro.GHC (to, ix, (^.), (^?!), (&), (<&>), (?~), (.~))

import qualified Piece
import qualified Peer
import qualified Bencode
import Base

---------------

data MetaInfo = Meta
  { _mAnnounce     :: String            -- the URL of the tracker
  , _mInfoHash     :: Piece.SHA1              -- a hash of info dictionary
  , _mInfo         :: TorrentInfo       -- a dictionary describing the torrent
  } deriving (Show, Eq)

data TorrentInfo = TorrentInfo
  { _tName            :: Maybe String    -- the suggested name for the file/directory
  , _tPLen            :: PLength         -- the number of bytes in each piece of the torrent
  , _tPHash           :: Piece.SHA1            -- SHA1 hashes of the pieces
  , _tDLen            :: DLength         -- length of the file in bytes (could also be files key for multi-file torrent)
  } deriving (Show, Eq)

$(makeLenses ''MetaInfo)
$(makeLenses ''TorrentInfo)

promptName :: TorrentInfo -> IO TorrentInfo
promptName t = do
  nm <- putStrLn "Please name the torrent." >> getLine
  return $ t & tName ?~ nm

getHash :: MetaInfo -> PIndex -> Piece.SHA1
getHash meta pIndex =
  let startIndex = 20 * pIndex
      endIndex = startIndex + 20 in
  Piece.SHA1 $ takeIn startIndex endIndex $ ( Piece.getSHA1 $ meta ^. mInfo . tPHash )
  where
    takeIn :: Index -> Index -> B.ByteString -> B.ByteString
    takeIn start end = B.take end . B.drop start

data TrackerResponse = TrackerResponse
  { _trInterval         :: Int
  , _trID               :: String
  , _trComplete         :: Int
  , _trIncomplete       :: Int
  , _trPeers            :: Either [Peer.Config] [Peer.Handle]
  } deriving (Eq)

$(makeLenses ''TrackerResponse)

newTrackerResponse :: Int -> ID -> Int -> Int -> [Peer.Config] -> TrackerResponse
newTrackerResponse _trInterval _trID _trComplete _trIncomplete _trPeers = TrackerResponse{ _trPeers = Left _trPeers, .. }

parseTrackerResponse :: AP.Parser TrackerResponse
parseTrackerResponse = do
  d <- Bencode.dictionaryParser
  case
    sequenceT  ( d HM.!? "interval"    >>= Bencode.intMb
               , d HM.!? "tracker_id"  >>= Bencode.stringMb
               , d HM.!? "complete"    >>= Bencode.intMb
               , d HM.!? "incomplete"  >>= Bencode.intMb
               , d HM.!? "peers"       >>= getPeers
               )
    of Just (_trInterval, _trID, _trComplete, _trIncomplete, _trPeers)
         -> return TrackerResponse{ .. }
       Nothing
         -> fail "Failed to find key in tracker response dictionary."
  where
    getPeers :: Bencode.BenValue -> Maybe ( Either [Peer.Config] [Peer.Handle] )
    getPeers bv = Bencode.listMb bv >>= ( sequence . map getPeer ) >>= ( Just . Left )

    getPeer :: Bencode.BenValue -> Maybe Peer.Config
    getPeer bv = do
      d     <- Bencode.dictionaryMb bv
      _hID   <- d HM.!? "peer_id" >>= Bencode.stringMb
      _hIP   <- d HM.!? "ip"      >>= Bencode.stringMb
      _hPort <- d HM.!? "port"    >>= Bencode.intMb
      return Peer.Config{ .. }

instance Serialize TrackerResponse where
  encode = undefined
  decode = formatAP parseTrackerResponse

initPeers :: MetaInfo -> TrackerResponse -> IO TrackerResponse
initPeers meta tr
  | tr ^. trPeers . to isLeft = let Left pLeft = tr ^. trPeers in
    do ps <- sequence $ map ( meta ^. mInfo . tDLen . to Left . to Peer.newFromConfig ) pLeft
       return $ tr & trPeers .~ Right ps
  | otherwise = return tr

data Handle = Handle
  { _hMeta               :: MetaInfo
  , _hDownloaded         :: MVar Int
  , _hUploaded           :: MVar Int
  , _hTrackerResponse    :: TrackerResponse
  , _hPieces             :: IOArray PIndex Piece.Handle
  , _hChannel            :: Chan Piece.Handle
  , _hBitfield           :: MVarBitfield
  , _hRoot               :: FilePath
  } deriving (Eq)
  
$(makeLenses ''Handle)

new :: MetaInfo -> TrackerResponse -> FilePath -> IO Handle
new _hMeta _hTrackerResponse _hRoot =
  let
    l = ceilDiv ( _hMeta ^. mInfo . tDLen ) ( _hMeta ^. mInfo . tPLen )
    pieces = MA.newListArray (0, l) =<< sequence [ _hMeta ^. mInfo . tPLen . to ( Piece.new i ) | i <- [0..l] ]
    _hBitfield = newMVarBF l
  in Handle _hMeta
     <$> newMVar 0
     <*> newMVar 0
     <*> return _hTrackerResponse
     <*> pieces
     <*> newChan
     <*> _hBitfield
     <*> return _hRoot
  where
    ceilDiv :: Int -> Int -> Int
    ceilDiv a b = -div ( -a ) b

getPath :: Handle -> Maybe FilePath
getPath h = ( h ^. hRoot ++ ) <$> h ^. hMeta . mInfo . tName

writeBlock :: Handle -> Piece.Block -> IO ()
writeBlock h b =
  let pIO = h ^. hPieces
      pi  = b ^. Piece.pIndex in
  do piece <- MA.readArray pIO pi :: IO Piece.Handle
     Piece.writeBlock piece ( h ^. hChannel ) b

verify :: Handle -> Piece.Blocks -> IO Bool
verify h b = ( Piece.collectBlocks b <&> Piece.newSHA1 ) >>= return . ( == getHash ( h ^. hMeta ) ( b ^. Piece.hIndex ) )

processChan :: Handle -> IO ()
processChan h = do
  ph@( Piece.Incomplete blocks ) <- readChan $ h ^. hChannel -- only incomplete pieces in Channel ( unsafe )
  let pi = blocks ^. Piece.hIndex
  validHash <- verify h blocks
  when validHash $
    do modifyMVar_ ( h ^?! hBitfield . ix pi ) ( const $ return True )
       modifyMVar_ ( h ^. hDownloaded )        ( return . ( blocks ^. Piece.hSize + ) )
       join $ MA.writeArray ( h ^. hPieces ) pi <$> Piece.complete ph

writePiecesUnsafe :: Handle -> IO ()
writePiecesUnsafe h = do
  xs  <- getElems ( h ^. hPieces )
  B.writeFile ( h ^. hRoot ) $ B.concat [ x | Piece.Complete x <- xs ]
