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

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad

import Lens.Micro.TH
import Lens.Micro.GHC (to, ix, (^.), (^?!), (&), (<&>), (?~), (.~))

import qualified Piece
import qualified Peer
import qualified Bencode
import Types

---------------

data MetaInfo = Meta
  { _announce     :: String            -- the URL of the tracker
  , _infoHash     :: Piece.SHA1              -- a hash of info dictionary
  , _info         :: TorrentInfo       -- a dictionary describing the torrent
  } deriving (Show, Eq)

data TorrentInfo = TorrentInfo
  { _name            :: Maybe String    -- the suggested name for the file/directory
  , _piecesLength    :: PLength         -- the number of bytes in each piece of the torrent
  , _pieces          :: Piece.SHA1            -- SHA1 hashes of the pieces
  , _length          :: DLength         -- length of the file in bytes (could also be files key for multi-file torrent)
  } deriving (Show, Eq)

$(makeLenses ''MetaInfo)
$(makeLenses ''TorrentInfo)

promptName :: TorrentInfo -> IO TorrentInfo
promptName t = do
  nm <- putStrLn "Please name the torrent." >> getLine
  return $ t & name ?~ nm

getHash :: MetaInfo -> PIndex -> Piece.SHA1
getHash meta pIndex =
  let startIndex = 20 * pIndex
      endIndex = startIndex + 20 in
  Piece.SHA1 $ takeIn startIndex endIndex $ ( Piece.getSHA1 $ meta ^. info . pieces )
  where
    takeIn :: Index -> Index -> B.ByteString -> B.ByteString
    takeIn start end = B.take end . B.drop start

data TrackerResponse = TrackerResponse
  { _interval         :: Int
  , _trackerID        :: String
  , _complete         :: Int
  , _incomplete       :: Int
  , _peers            :: Either [Peer.Config] [Peer.Handle]
  } deriving (Eq)

$(makeLenses ''TrackerResponse)

initPeers :: MetaInfo -> TrackerResponse -> IO TrackerResponse
initPeers meta tr
  | tr ^. peers . to isLeft = let Left pLeft = tr ^. peers in
    do ps <- sequence $ map ( meta ^. info . length . to Left . to Peer.newFromConfig ) pLeft
       return $ tr & peers .~ Right ps
  | otherwise = return tr

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
    of Just (_interval, _trackerID, _complete, _incomplete, _peers)
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

data Handle = Handle
  { _hMeta               :: MetaInfo
  , _hDownloaded         :: MVar Int
  , _hUploaded           :: MVar Int
  , _hTrackerResponse    :: TrackerResponse
  , _hPieces             :: IOArray PIndex Piece.Handle
  , _hChannel            :: Chan Piece.Handle
  , _hBitfield           :: Bitfield
  , _hRoot               :: FilePath
  } deriving (Eq)
  
$(makeLenses ''Handle)

new :: MetaInfo -> TrackerResponse -> FilePath -> IO Handle
new _hMeta _hTrackerResponse _hRoot =
  let
    l = ceilDiv ( _hMeta ^. info . length ) ( _hMeta ^. info . piecesLength )
  in do
    _hPieces     <- MA.newListArray (0, l)
                     =<< sequence [ _hMeta ^. info . piecesLength . to ( Piece.new i ) | i <- [0..l] ]
    _hBitfield   <- Piece.newBitfield l          
    _hDownloaded <- newMVar 0
    _hUploaded   <- newMVar 0
    _hChannel    <- newChan
    return Handle{..}
  where
    ceilDiv :: Int -> Int -> Int
    ceilDiv a b = -div ( -a ) b

getPath :: Handle -> Maybe FilePath
getPath h = ( h ^. hRoot ++ ) <$> h ^. hMeta . info . name

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
