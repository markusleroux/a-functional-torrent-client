-- | 
module Bencode where

import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Char8 as BS8

import qualified Data.Attoparsec.ByteString as AP
import Data.Attoparsec.ByteString.Char8 (char, signed, decimal)

import Lens.Micro.Platform ((<&>))

------------------

data BenValue
  = BenString B.ByteString
  | BenInt Int
  | BenList [BenValue]
  | BenMap ( HM.HashMap B.ByteString BenValue )
    deriving (Show, Eq)

byteStringParser :: AP.Parser B.ByteString
byteStringParser = decimal >>= ( \ n -> char ':' >> AP.take n )

benStringParser :: AP.Parser BenValue
benStringParser = byteStringParser <&> BenString

benIntParser :: AP.Parser BenValue
benIntParser = char 'i' *> signed decimal <* char 'e' <&> BenInt

benListParser :: AP.Parser BenValue
benListParser = char 'l' *> AP.many' benValueParser <* char 'e' <&> BenList

dictionaryParser :: AP.Parser (HM.HashMap B.ByteString BenValue)
dictionaryParser = char 'd' *> AP.many' pair <* char 'e' <&> HM.fromList
  where
    pair :: AP.Parser (B.ByteString, BenValue)
    pair = (,) <$> byteStringParser <*> benValueParser

benDictionaryParser :: AP.Parser BenValue
benDictionaryParser = BenMap <$> dictionaryParser

benValueParser :: AP.Parser BenValue
benValueParser = AP.choice [benStringParser, benIntParser, benListParser, benDictionaryParser]

----------------

intMb :: BenValue -> Maybe Int
intMb ( BenInt n ) = Just n
intMb _ = Nothing

listMb :: BenValue -> Maybe [BenValue]
listMb ( BenList xs ) = Just xs
listMb _ = Nothing

bstringMb :: BenValue -> Maybe B.ByteString
bstringMb ( BenString s ) = Just s
bstringMb _ = Nothing

stringMb :: BenValue -> Maybe String
stringMb = fmap BS8.unpack . bstringMb

dictionaryMb :: BenValue -> Maybe ( HM.HashMap B.ByteString BenValue )
dictionaryMb ( BenMap hm ) = Just hm
dictionaryMb _ = Nothing

-------------
stringParser :: Int -> AP.Parser String
stringParser n = AP.take n >>= ( return . BS8.unpack )
