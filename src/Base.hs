-- |
{-# LANGUAGE TemplateHaskell #-}

module Base where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.MVar

import qualified Crypto.Hash.SHA1 as SHA1

import Data.Bits hiding (clearBit)
import Data.Bits.Bitwise (toListBE)
import Data.Hashable
import qualified Data.Array as Arr
import qualified Data.ByteString as B
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Lazy (toStrict)
import Data.IORef

import Lens.Micro.Platform (_Right, to, (^?))

-----------------

type IP      = String
type Port    = Int
type ID      = String

type Index = Int
type Length = Int

type DLength = Int
 
type PIndex = Int
type PLength = Int

type BIndex = Int
type BLength = Int
      
-----------------

type Bitfield = Arr.Array Int Bool

newBF :: Length -> Bitfield
newBF n = Arr.listArray (0, n) $ replicate n False

bfFromBS :: B.ByteString -> Bitfield
bfFromBS bs = let bl = concatMap toListBE $ B.unpack bs in
  Arr.listArray (0, length bl - 1) bl

class HasBitfield a where
  getBF :: a -> IORef Bitfield

-----------------

newtype SHA1 = SHA1 { getSHA1 :: B.ByteString } deriving (Eq)

instance Show SHA1 where
  show = BS8.unpack . getSHA1

instance Hashable SHA1 where
  hashWithSalt salt = hashWithSalt salt . getSHA1

newSHA1 :: B.ByteString -> SHA1
newSHA1 = SHA1 . SHA1.hash

class HasInfoHash a where
  getInfoHash :: a -> SHA1

instance HasInfoHash SHA1 where
  getInfoHash = id

-----------------

type MVarBitfield = Arr.Array Int ( MVar Bool )

newMVarBF :: MonadIO m => Length -> m MVarBitfield
newMVarBF n = liftIO $ Arr.listArray (0, n) <$> replicateM n ( newMVar False )

-- does this work?
clearMVarBitfield :: MonadIO m => MVarBitfield -> m ()
clearMVarBitfield = liftIO . sequence_ . fmap clearBit
  where
    clearBit :: MVar Bool -> IO ()
    clearBit bMVar = void $ swapMVar bMVar False

encodeMVarBF :: MonadIO m => MVarBitfield -> m B.ByteString
encodeMVarBF arr = liftIO $ do
  bs <- sequence . map readMVar . Arr.elems $ arr
  return . toStrictBS $ mconcat [ BB.int32BE . fromIntegral $ length arr + 1
    , BB.int8 5
    , encodeBools bs
    ]
  where
    encodeBools :: [Bool] -> BB.Builder
    encodeBools [] = mempty
    encodeBools bs = let (eight, rest) = splitAt 8 bs in
      ( BB.word8 . foldl (.|.) zeroBits . fmap bit . filter ( eight !! ) $ [0..8] ) <> encodeBools rest

-----------------

formatAP :: AP.Parser a -> B.ByteString -> Maybe a
formatAP parser = ( ^? to ( AP.parseOnly parser ) . _Right )

toStrictBS :: BB.Builder -> B.ByteString
toStrictBS = toStrict . BB.toLazyByteString

parseInt32, parseLengthPrefix :: AP.Parser Int
parseInt32 = AP.take 4 >>= ( return . read . BS8.unpack )
parseLengthPrefix = parseInt32

both :: Either a a -> a
both ( Left v ) = v
both ( Right v ) = v
