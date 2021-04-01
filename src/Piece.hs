-- | 
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing -fno-warn-unused-matches #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards, GeneralizedNewtypeDeriving, DataKinds #-}

module Piece where

import Data.Word
import Data.Array (Array, listArray, elems)
import Data.Array.IO (IOArray)
import Data.Bits.Bitwise (toListBE)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Builder as BB

import qualified Crypto.Hash.SHA1 as SHA1

import Lens.Micro.GHC (ix, strict, (^.), (^?), (<&>))
import Lens.Micro.TH

import Control.Monad
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Data.Array.MArray

-------------

class Serialize a where
  encode :: a -> B.ByteString

newtype SHA1 = SHA1 { getSHA1 :: B.ByteString } deriving (Eq)

instance Show SHA1 where
  show = BS8.unpack . getSHA1

newSHA1 :: B.ByteString -> SHA1
newSHA1 = SHA1 . SHA1.hash

type Bitfield = Array Int ( MVar Bool )

newBitfield :: Int -> IO Bitfield
newBitfield n = listArray (0, n) <$> replicateM n newEmptyMVar

newBitfieldBS :: B.ByteString -> Array Int Bool
newBitfieldBS bs = let bl = concatMap toListBE $ B.unpack bs in
  listArray (0, length bl - 1) bl

-- does this work?
clearBitfield :: Bitfield -> IO ()
clearBitfield = sequence_ . fmap clearBit
  where
    clearBit :: MVar Bool -> IO ()
    clearBit bMVar = void $ swapMVar bMVar False

clearIOArray :: IOArray Int e -> e -> IO ()
clearIOArray arr val = void $ mapArray ( const val ) arr
 
type Index = Int
type Length = Int
type BIndex = Int
type BLength = Int

data Block = Block
  { _bIndex   :: BIndex
  , _bLength  :: BLength
  , _pIndex   :: Index
  , _bData    :: B.ByteString
  } deriving (Show, Eq)

$(makeLenses ''Block)

instance Serialize Block where
  encode b = ( ^. strict ) . BB.toLazyByteString . mconcat $
    [ BB.int32BE . fromIntegral $ 9 + ( B.length $ b ^. bData )
    , BB.int32BE . fromIntegral $ b ^. pIndex
    , BB.int32BE . fromIntegral $ b ^. bIndex
    , BB.byteString $ b ^. bData
    ]

newBlock :: BIndex -> BLength -> Index -> Block
newBlock _bIndex _bLength _pIndex = Block { _bData = B.empty, .. }

data Blocks = Blocks
  { _hIndex       :: Index                  -- index of the piece within download
  , _hSize        :: Length                 -- size of the piece in bytes
  , _hBlocks      :: IOArray Int Word8      -- array of data (update only after write to _pieceBitfield)
  , _hHash        :: SHA1                   -- correct hash of the piece (from MetaInfo)
  , _hBitfield    :: Bitfield               -- indicate if data has been written
  } deriving (Eq)

$(makeLenses ''Blocks)

instance Show Blocks where
  show p = concat [ "Piece ", show $ p ^. hIndex, ": ", show $ p ^. hSize, " blocks."]

collectBlocks :: Blocks -> IO B.ByteString
collectBlocks h = B.pack <$> getElems ( h ^. hBlocks )

data Handle = Complete B.ByteString | Incomplete Blocks

$(makeLenses ''Handle)

new :: Index -> Length -> SHA1 -> IO Handle
new _hIndex _hSize _hHash =
  do _hBlocks   <- newListArray (0, _hSize) [ 0 | j <- [0.._hSize]]
     _hBitfield <- newBitfield _hSize
     return $ Incomplete Blocks{..}

verify :: Blocks -> IO Bool
verify b = ( Piece.collectBlocks b <&> Piece.newSHA1 ) >>= return . ( b ^. hHash == )

complete :: Handle -> IO Handle
complete ( Incomplete h ) = Complete <$> collectBlocks h
complete c = return c

tryAddBlock :: Handle -> Chan Handle -> Block -> IO ()
tryAddBlock h@( Incomplete piece ) chan block = do
  addBlock ( block ^. bData ) ( block ^. bIndex ) ( block ^. bLength )
  isFinishedPiece <- and <$> ( sequence $ map readMVar $ elems bf )
  when isFinishedPiece $ writeChan chan h
  where
    addBlock :: B.ByteString -> BIndex -> BLength -> IO ()
    addBlock b i l
      | l > 0 = do
          isWritten <- maybe ( const $ return False ) swapMVar ( bf ^? ix i ) True
          unless isWritten $ writeArray blocks i ( B.head b )
          addBlock ( B.tail b ) ( i + 1 ) ( l - 1 )
      | otherwise = return ()
  
    blocks = piece ^. hBlocks
    bf = piece ^. hBitfield
tryAddBlock _ _ _ = return ()

