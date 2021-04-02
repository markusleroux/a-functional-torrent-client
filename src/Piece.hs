-- | 
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing -fno-warn-unused-matches #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards #-}

module Piece where

import Data.Word 
import Data.Bits
import Data.Array (Array, listArray, elems)
import Data.Array.IO (IOArray)
import Data.Array.MArray
import Data.Bits.Bitwise (toListBE)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Builder as BB

import qualified Crypto.Hash.SHA1 as SHA1

import Lens.Micro.GHC (to, ix, strict, (^.), (^?), (&))
import Lens.Micro.TH

import Control.Monad
import Control.Concurrent.Chan
import Control.Concurrent.MVar

import Types

-------------

newtype SHA1 = SHA1 { getSHA1 :: B.ByteString } deriving (Eq)

instance Show SHA1 where
  show = BS8.unpack . getSHA1

newSHA1 :: B.ByteString -> SHA1
newSHA1 = SHA1 . SHA1.hash

newBitfield :: Length -> IO Bitfield
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

encodeBF :: Bitfield -> IO B.ByteString
encodeBF arr = do
  bs <- sequence . map readMVar . elems $ arr
  [ BB.int32BE . fromIntegral $ length arr + 1
    , BB.int8 5
    , encodeBools bs 0
    ] & return . ( ^. strict ) . BB.toLazyByteString . mconcat
  where
    encodeBools :: [Bool] -> Index -> BB.Builder
    encodeBools [] i = mempty
    encodeBools bs i = let (eight, rest) = splitAt 8 bs in
      ( BB.word8 . foldl (.|.) zeroBits . fmap bit . filter ( eight !! ) $ [0..8] ) <> encodeBools rest 8

clearIOArray :: IOArray Int e -> e -> IO ()
clearIOArray arr val = void $ mapArray ( const val ) arr

data Block = Block
  { _bIndex   :: BIndex
  , _bLength  :: BLength
  , _pIndex   :: PIndex
  , _bData    :: B.ByteString
  } deriving (Show, Eq)

$(makeLenses ''Block)

instance Serialize Block where
  encode b = ( ^. strict ) . BB.toLazyByteString . mconcat $
    [ b ^. bData . to B.length . to ( + 9 ) . to fromIntegral . to BB.int32BE
    , BB.int32BE 7
    , b ^. pIndex . to fromIntegral . to BB.int32BE
    , b ^. bIndex . to fromIntegral . to BB.int32BE
    , b ^. bData  . to BB.byteString
    ]

newBlock :: BIndex -> BLength -> PIndex -> Block
newBlock _bIndex _bLength _pIndex = Block { _bData = B.empty, .. }

data Blocks = Blocks
  { _hIndex       :: PIndex                  -- index of the piece within download
  , _hSize        :: PLength                -- size of the piece in bytes
  , _hBlocks      :: IOArray Int Word8      -- array of data (update only after write to _pieceBitfield)
  , _hBitfield    :: Bitfield               -- indicate if data has been written
  } deriving (Eq)

$(makeLenses ''Blocks)

instance Show Blocks where
  show p = concat [ "Piece ", show $ p ^. hIndex, ": ", show $ p ^. hSize, " blocks."]

collectBlocks :: Blocks -> IO B.ByteString
collectBlocks h = B.pack <$> ( h ^. hBlocks . to getElems  )

data Handle = Complete B.ByteString | Incomplete Blocks

new :: Index -> PLength -> IO Handle
new _hIndex _hSize =
  do _hBlocks   <- newListArray (0, _hSize) [ 0 | _ <- [0.._hSize]]
     _hBitfield <- newBitfield _hSize
     return $ Incomplete Blocks{..}

complete :: Handle -> IO Handle
complete ( Incomplete h ) = Complete <$> collectBlocks h
complete h = return h

writeBlock :: Handle -> Chan Handle -> Block -> IO ()
writeBlock h@( Incomplete piece ) chan block = do
  addBlock ( block ^. bData ) ( block ^. bIndex ) ( block ^. bLength )
  isFinishedPiece <- and <$> ( sequence . map readMVar . elems $ piece ^. hBitfield )
  when isFinishedPiece $ writeChan chan h
  where
    addBlock :: B.ByteString -> BIndex -> BLength -> IO ()
    addBlock b i l
      | l > 0 =
          do isWritten <- maybe ( const $ return False ) swapMVar ( piece ^? hBitfield . ix i ) True
             unless isWritten $ writeArray ( piece ^. hBlocks ) i ( B.head b )
             addBlock ( B.tail b ) ( i + 1 ) ( l - 1 )
      | otherwise = return ()
writeBlock _ _ _  = return ()
