-- | 
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing -fno-warn-unused-matches #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards #-}

module Piece where

import Data.Word 
import Data.Array (elems)
import Data.Array.IO (IOArray)
import Data.Array.MArray

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Builder as BB
import qualified Data.Attoparsec.ByteString as AP

import qualified Crypto.Hash.SHA1 as SHA1

import Lens.Micro.GHC (to, ix, (^.), (^?))
import Lens.Micro.TH

import Control.Monad
import Control.Concurrent.Chan
import Control.Concurrent.MVar

import Base

-------------

newtype SHA1 = SHA1 { getSHA1 :: B.ByteString } deriving (Eq)

instance Show SHA1 where
  show = BS8.unpack . getSHA1

newSHA1 :: B.ByteString -> SHA1
newSHA1 = SHA1 . SHA1.hash

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
  decode = formatAP parseBlock
  encode block = toStrictBS $ mconcat
    [ block ^. bData . to B.length . to ( + 9 ) . to fromIntegral . to BB.int32BE
    , BB.int32BE 7
    , block ^. pIndex . to fromIntegral . to BB.int32BE
    , block ^. bIndex . to fromIntegral . to BB.int32BE
    , block ^. bData  . to BB.byteString
    ]

parseBlock :: AP.Parser Piece.Block
parseBlock = do
    _bLength <- parseLengthPrefix
    void $ AP.word8 7
    _pIndex  <- parseLengthPrefix
    _bIndex  <- parseLengthPrefix
    _bData   <- AP.take ( _bLength - 9 )
    return $ Piece.Block{..}
  
newBlock :: BIndex -> BLength -> PIndex -> Block
newBlock _bIndex _bLength _pIndex = Block { _bData = B.empty, .. }

data Blocks = Blocks
  { _hIndex       :: PIndex                  -- index of the piece within download
  , _hSize        :: PLength                -- size of the piece in bytes
  , _hBlocks      :: IOArray Int Word8      -- array of data (update only after write to _pieceBitfield)
  , _hBitfield    :: MVarBitfield               -- indicate if data has been written
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
     _hBitfield <- newMVarBF _hSize
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
