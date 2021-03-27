-- | 
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing -fno-warn-unused-matches #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Piece
  ( SHA1(..), encodeSHA1, showSHA1
  , BIndex
  , BLength
  , Block
  , Bitfield
  , PIndex
  , PLength
  , Piece(..), pieceIndex, pieceSize, pieceBlocks, pieceHash, pieceBitfield, pieceDone, unpack, clearBitfield, collectBlocks
  ) where

import Data.Array (Array, listArray, elems)
import Data.Array.IO (IOArray)
import Data.Bits.Bitwise (toListBE)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T
import qualified Data.ByteString as B

import qualified Crypto.Hash.SHA1 as SHA1

import Lens.Micro.GHC (ix, (^.), (^?))
import Lens.Micro.TH

import Control.Monad
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Data.Array.MArray

-------------

newtype SHA1 = SHA1 { getSHA1 :: B.ByteString } deriving (Show, Eq)

encodeSHA1 :: B.ByteString -> SHA1
encodeSHA1 = SHA1 . SHA1.hash

showSHA1 :: SHA1 -> T.Text
showSHA1 = decodeUtf8 . getSHA1

type Bitfield = Array Int ( MVar Bool )

createBitfield :: Int -> IO Bitfield
createBitfield n = fmap ( listArray (0, n) ) $ replicateM n newEmptyMVar

-- does this work?
clearBitfield :: Bitfield -> IO ()
clearBitfield = sequence_ . fmap clearBit
  where
    clearBit :: MVar Bool -> IO ()
    clearBit bMVar = void $ swapMVar bMVar False

clearIOArray :: e -> IOArray Int e -> IO ()
clearIOArray val = void . mapArray ( const val )

unpack :: B.ByteString -> Array Int Bool
unpack bs = let bl = concatMap toListBE $ B.unpack bs in
  listArray (0, length bl - 1) bl

type BIndex = Int
type BLength = Int
type Block = B.ByteString
 
type PIndex = Int
type PLength = Int

-----------------

data Piece = Piece
  { _pieceIndex       :: PIndex
  , _pieceSize        :: PLength
  , _pieceBlocks      :: IOArray BIndex Block      -- update only after write to _pieceBitfield
  , _pieceHash        :: SHA1
  , _pieceBitfield    :: Bitfield
  , _pieceDone        :: MVar Bool                 -- update only after write to _pieceBitfield
  } deriving (Eq)

$(makeLenses ''Piece)

tryAddBlock :: Chan Piece -> Piece -> BIndex -> Block -> IO ()
tryAddBlock chan piece bindex block = do
  maybe ( const $ return False ) swapMVar ( bf ^? ( ix bindex ) ) True >>= ( flip unless addBlock )
  where
    addBlock :: IO ()
    addBlock = do
      writeArray blocks bindex block
      b <- fmap and $ sequence $ map readMVar $ elems bf
      when b $ writeChan chan piece
  
    blocks = piece ^. pieceBlocks
    bf = piece ^. pieceBitfield

collectBlocks :: IOArray BIndex Block -> IO B.ByteString
collectBlocks = fmap B.concat . getElems

{-
tryAddBlock :: Piece -> BIndex -> Block -> IO Bool
tryAddBlock piece bindex block = do
  b <- maybe ( const $ return False ) swapMVar ( bf ^? ( ix bindex ) ) True
  if b then addBlock else return False
  where
    blocks = piece ^. pieceBlocks
    bf = piece ^. pieceBitfield

    addBlock :: IO Bool
    addBlock = writeArray blocks bindex block >> ( fmap and $ sequence $ map readMVar $ elems bf )

tryAddBlock :: Piece -> BIndex -> Block -> IO ()
tryAddBlock p bi block = ( readMVar $ p ^. pieceDone ) >>= ( flip unless addBlock )
  where
    addBlock :: IO ()
    addBlock = do
      pBF <- takeMVar $ p ^. pieceBitfield
      let isEmptyMb = pBF ^? ix bi
      if isJust isEmptyMb && fromJust isEmptyMb
        then do
          let newpBF = pBF & ix bi .~ True
          when ( and newpBF ) ( void $ swapMVar ( p ^. pieceDone ) True )
          putMVar ( p ^. pieceBitfield ) newpBF
          writeArray ( p ^. pieceBlocks ) bi block
        else do
          putMVar ( p ^. pieceBitfield ) pBF

-----------------

addBlock :: BIndex -> Block -> IOArray BIndex Block -> Array Int ( MVar Bool ) -> IO Bool
addBlock bi block blocks bfMVar
  | bi < ( fst $ bounds bfMVar ), bi > ( snd $ bounds bfMVar ) = do
    b <- modifyMVar ( bfMVar ^?! ( ix bi ) ) ( \ b -> return (True, b) )
    if b then return False
      else do
        writeArray blocks bi block
        fmap and $ sequence $ map readMVar $ elems bfMVar
  | otherwise = return False

-----------------

addBlock' :: BIndex -> Block -> IOArray BIndex Block -> TArray Int Bool -> IO Bool
addBlock' bi block blocks bfT = do
  atomically addBlock''
  where
    addBlock'' :: STM Bool
    addBlock'' = do
      b <- unsafeRead bfT bi
      unsafeWrite bfT bi True
      if b then return False
        else do
          writeArray blocks bi block
          fmap and $ getElems bfT

---------------
-}

