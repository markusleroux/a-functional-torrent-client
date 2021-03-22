-- | 
module Bencode where

import Data.Int (Int64)
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Char8 as BS8

import qualified Data.Attoparsec.ByteString as AP
import Data.Attoparsec.ByteString.Char8 (char, signed, decimal)

import Lens.Micro.GHC ((<&>))

data BenValue = BenString B.ByteString
              | BenInt Int64
              | BenList [BenValue]
              | BenMap ( HM.HashMap B.ByteString BenValue )
    deriving (Show, Eq)

byteStringParser :: AP.Parser B.ByteString
byteStringParser = decimal >>= ( \ n -> char ':' >> AP.take n )

benStringParser :: AP.Parser BenValue
benStringParser = BenString <$> byteStringParser

intParser :: AP.Parser Int64
intParser = char 'i' *> signed decimal <* char 'e'

benIntParser :: AP.Parser BenValue
benIntParser = BenInt <$> intParser

listParser :: AP.Parser [BenValue]
listParser = char 'l' *> AP.many' benValueParser <* char 'e'

benListParser :: AP.Parser BenValue
benListParser = BenList <$> listParser

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

intMb :: Maybe BenValue -> Maybe Int64
intMb ( Just ( BenInt n ) ) = Just n
intMb _ = Nothing

listMb :: Maybe BenValue -> Maybe [BenValue]
listMb ( Just ( BenList xs ) ) = Just xs
listMb _ = Nothing

bstringMb :: Maybe BenValue -> Maybe B.ByteString
bstringMb ( Just ( BenString s ) ) = Just s
bstringMb _ = Nothing

stringMb :: Maybe BenValue -> Maybe String
stringMb = fmap BS8.unpack . bstringMb

dictionaryMb :: Maybe BenValue -> Maybe ( HM.HashMap B.ByteString BenValue )
dictionaryMb ( Just ( BenMap hm ) ) = Just hm
dictionaryMb _ = Nothing
