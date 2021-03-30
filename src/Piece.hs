-- | 
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing -fno-warn-unused-matches #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards, GeneralizedNewtypeDeriving #-}

module Piece where

import Data.Array (Array, listArray, elems)
import Data.Array.IO (IOArray)
import Data.Bits.Bitwise (toListBE)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Builder as BB

import qualified Crypto.Hash.SHA1 as SHA1

import Lens.Micro.GHC (ix, (^.), (^?), strict)
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
  encode b = ( ^. strict ) $ BB.toLazyByteString $ mconcat
    [ BB.int32BE $ fromIntegral ( 9 + ( B.length $ b ^. bData ) )
    , BB.int32BE $ fromIntegral $ b ^. pIndex
    , BB.int32BE $ fromIntegral $ b ^. bIndex
    , BB.byteString $ b ^. bData
    ]

newBlock :: BIndex -> BLength -> Index -> Block
newBlock _bIndex _bLength _pIndex = Block { _bData = B.empty, .. }

data Handle = Handle
  { _hIndex       :: Index
  , _hSize        :: Length
  , _hBlocks      :: IOArray Int Block      -- update only after write to _pieceBitfield
  , _hHash        :: SHA1
  , _hBitfield    :: Bitfield
  , _hDone        :: MVar Bool                 -- update only after write to _pieceBitfield
  } deriving (Eq)

$(makeLenses ''Handle)

instance Show Handle where
  show h = concat [ "Piece ", show $ h ^. hIndex, ": ", show $ h ^. hSize, " blocks."]

-- how to determine block length? Is it even fixed?
new :: Index -> Length -> BLength -> SHA1 -> IO Handle
new i size bLength hash =
  do blocks <- newListArray (0, size) [ newBlock j bLength i | j <- [0..size]]
     done <- newMVar False
     bf <- newBitfield bLength
     return Handle
       { _hIndex    = i
       , _hSize     = size
       , _hBlocks   = blocks
       , _hHash     = hash
       , _hBitfield = bf
       , _hDone     = done
       }

tryAddBlock :: Handle -> Chan Handle -> Block -> IO ()
tryAddBlock piece chan block = do
  maybe ( const $ return False ) swapMVar ( bf ^? ( ix $ block ^. bIndex ) ) True >>= ( flip unless addBlock )
  where
    addBlock :: IO ()
    addBlock = do
      writeArray blocks ( block ^. bIndex ) block
      b <- and <$> ( sequence $ map readMVar $ elems bf )
      when b $ writeChan chan piece
  
    blocks = piece ^. hBlocks
    bf = piece ^. hBitfield

collectBlocks :: Handle -> IO B.ByteString
collectBlocks = fmap ( B.concat . getDataList ) . getElems . ( ^. hBlocks )
  where
    getDataList :: [Block] -> [B.ByteString]
    getDataList = map ( ^. bData )

