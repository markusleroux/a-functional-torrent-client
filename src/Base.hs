-- | 

module Base where

import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Concurrent.MVar

import Data.Bits hiding (clearBit)
import Data.Bits.Bitwise (toListBE)
import qualified Data.Array as Arr
import qualified Data.ByteString as B
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Lazy (toStrict)

import Lens.Micro.GHC (_Right, (^?))

import qualified Network.Simple.TCP as TCP

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

class Serialize a where
  encode  :: a -> B.ByteString
  --decode :: B.ByteString -> Maybe a -- parseOnly $ AP.Parser a
  decode  :: AP.Parser a

receiveTCP :: (TCP.Socket, TCP.SockAddr) -> AP.Parser a -> MaybeT IO a
receiveTCP (sock, _) parser = do
  lenRaw <- MaybeT $ TCP.recv sock 4
  len    <- MaybeT . return $ AP.parseOnly parseLengthPrefix lenRaw ^? _Right
  hsRaw  <- MaybeT $ TCP.recv sock len
  MaybeT . return  $ ( AP.parseOnly parser ( lenRaw <> hsRaw ) ) ^? _Right

receiveTCPS :: Serialize a => (TCP.Socket, TCP.SockAddr) -> MaybeT IO a
receiveTCPS = flip receiveTCP decode

type Bitfield = Arr.Array Int Bool

newBF :: Length -> Bitfield
newBF n = Arr.listArray (0, n) $ replicate n False

bfFromBS :: B.ByteString -> Bitfield
bfFromBS bs = let bl = concatMap toListBE $ B.unpack bs in
  Arr.listArray (0, length bl - 1) bl

type MVarBitfield = Arr.Array Int ( MVar Bool )

newMVarBF :: Length -> IO MVarBitfield
newMVarBF n = Arr.listArray (0, n) <$> replicateM n ( newMVar False )

-- does this work?
clearMVarBitfield :: MVarBitfield -> IO ()
clearMVarBitfield = sequence_ . fmap clearBit
  where
    clearBit :: MVar Bool -> IO ()
    clearBit bMVar = void $ swapMVar bMVar False

encodeMVarBF :: MVarBitfield -> IO B.ByteString
encodeMVarBF arr = do
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

--formatAP :: AP.Parser a -> B.ByteString -> Maybe a
--formatAP parser = ( ^? to ( AP.parseOnly parser ) . _Right )
formatAP :: AP.Parser a -> AP.Parser a
formatAP = id

toStrictBS :: BB.Builder -> B.ByteString
toStrictBS = toStrict . BB.toLazyByteString

parseLengthPrefix :: AP.Parser Int
parseLengthPrefix = AP.take 4 >>= ( return . read . BS8.unpack )

parseInt32 :: AP.Parser Int
parseInt32 = parseLengthPrefix
