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
import Data.Maybe

import Control.Monad
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad.IO.Class

import Lens.Micro.TH
import Lens.Micro.Platform (to, ix, (^.), (^?!), (&), (<&>), (?~))

import Base
import Connection
import qualified Piece
import qualified Peer
import qualified Bencode

---------------
-- layer 1: MonadIO

data MetaInfo = Meta
  { _mAnnounce     :: String            -- the URL of the tracker
  , _mInfoHash     :: SHA1              -- a hash of info dictionary
  , _mInfo         :: TorrentInfo       -- a dictionary describing the torrent
  } deriving (Show, Eq)

data TorrentInfo = TorrentInfo
  { _tName            :: Maybe String    -- the suggested name for the file/directory
  , _tPLen            :: PLength         -- the number of bytes in each piece of the torrent
  , _tPHash           :: SHA1            -- SHA1 hashes of the pieces
  , _tDLen            :: DLength         -- length of the file in bytes (could also be files key for multi-file torrent)
  } deriving (Show, Eq)

$(makeLenses ''MetaInfo)
$(makeLenses ''TorrentInfo)

promptName :: MonadIO m => TorrentInfo -> m TorrentInfo
promptName t = do
  nm <- liftIO $ putStrLn "Please name the torrent." >> getLine
  return $ t & tName ?~ nm

getHash :: MetaInfo -> PIndex -> SHA1
getHash meta pIndex =
  let startIndex = 20 * pIndex
      endIndex = startIndex + 20
  in
    SHA1 $ takeIn startIndex endIndex $ getSHA1 $ meta ^. mInfo . tPHash
  where
    takeIn :: Index -> Index -> B.ByteString -> B.ByteString
    takeIn start end = B.take end . B.drop start

data TrackerResponse = TrackerResponse
  { _trInterval         :: Int
  , _trID               :: String
  , _trComplete         :: Int
  , _trIncomplete       :: Int
  , _trPeers            :: [Peer.Handle]
  } deriving (Eq)

$(makeLenses ''TrackerResponse)

trHasPeerID :: TrackerResponse -> ID -> Bool
trHasPeerID tr id = mapConfig $ tr ^. trPeers
  where
    hasID :: [ID] -> Bool
    hasID = elem id
    
    mapConfig :: [Peer.Handle] -> Bool
    mapConfig = hasID . catMaybes . map ( ^. Peer.cID )

_decodeTR :: HasInfoHash a => a -> AP.Parser TrackerResponse
_decodeTR v = do
  d <- Bencode.dictionaryParser
  case
    sequenceT  ( d HM.!? "interval"    >>= Bencode.intMb
               , d HM.!? "tracker_id"  >>= Bencode.stringMb
               , d HM.!? "complete"    >>= Bencode.intMb
               , d HM.!? "incomplete"  >>= Bencode.intMb
               , d HM.!? "peers"       >>= getPeers
               )
    of
      Just (_trInterval, _trID, _trComplete, _trIncomplete, _trPeers)
        -> return TrackerResponse{ .. }
      Nothing
        -> fail "Failed to find key in tracker response dictionary."
  where
    getPeers :: Bencode.BenValue -> Maybe [Peer.Handle]
    getPeers bv = Bencode.listMb bv >>= ( sequence . map getPeer )

    getPeer :: Bencode.BenValue -> Maybe Peer.Handle
    getPeer bv = let _hInfoHash = Left $ getInfoHash v in
      do
        d       <- Bencode.dictionaryMb bv
        let _cID = d HM.!? "peer_id" >>= Bencode.stringMb
        _cIP    <- d HM.!? "ip"      >>= Bencode.stringMb
        _cPort  <- d HM.!? "port"    >>= Bencode.intMb
        -- problem
        return Peer.Handle{ .. }

instance Decode TrackerResponse where
  decode = undefined --_decodeTR

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

new :: MonadIO m => MetaInfo -> TrackerResponse -> FilePath -> m Handle
new _hMeta _hTrackerResponse _hRoot =
  let
    l = ceilDiv ( _hMeta ^. mInfo . tDLen ) ( _hMeta ^. mInfo . tPLen )
    pieces = MA.newListArray (0, l) =<< sequence [ _hMeta ^. mInfo . tPLen . to ( Piece.new i ) | i <- [0..l] ]
    _hBitfield = newMVarBF l
  in
    liftIO $ Handle _hMeta
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

writeBlock :: MonadIO m => Handle -> Piece.Block -> m ()
writeBlock h b =
  let
    pIO = h ^. hPieces
    pi  = b ^. Piece.pIndex
  in liftIO $ do
    piece <- MA.readArray pIO pi
    Piece.writeBlock piece ( h ^. hChannel ) b

verifyPiece :: MonadIO m => Handle -> Piece.Blocks -> m Bool
verifyPiece h b = liftIO $ ( Piece.collectBlocks b <&> newSHA1 ) >>= return . ( == getHash ( h ^. hMeta ) ( b ^. Piece.hIndex ) )

processChan :: MonadIO m => Handle -> m ()
processChan h = liftIO $ do
  ph@( Piece.Incomplete blocks ) <- readChan $ h ^. hChannel -- only incomplete pieces in Channel ( unsafe )
  let pi = blocks ^. Piece.hIndex
  validHash <- verifyPiece h blocks
  when validHash $
    do modifyMVar_ ( h ^?! hBitfield . ix pi ) ( const $ return True )
       modifyMVar_ ( h ^. hDownloaded )        ( return . ( blocks ^. Piece.hSize + ) )
       join $ MA.writeArray ( h ^. hPieces ) pi <$> Piece.complete ph

writePiecesUnsafe :: MonadIO m => Handle -> m ()
writePiecesUnsafe h = liftIO $ do
  xs  <- getElems ( h ^. hPieces )
  B.writeFile ( h ^. hRoot ) $ B.concat [ x | Piece.Complete x <- xs ]

