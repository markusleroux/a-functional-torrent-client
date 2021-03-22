-- | 

{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Torrent where

import Data.Int (Int64)
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)

import qualified Data.ByteString as B

--import Lens.Micro.GHC
import Lens.Micro.TH

type Length = Int64

newtype SHA1 = SHA1 { getSHA1 :: B.ByteString } deriving (Show, Eq)

encodeSHA1 :: B.ByteString -> SHA1
encodeSHA1 = SHA1 . SHA1.hash

showSHA1 :: SHA1 -> T.Text
showSHA1 = decodeUtf8 . getSHA1

data MetaInfo = Meta
  { _announce     :: String            -- the URL of the tracker
  , _infoHash     :: SHA1              -- a hash of info dictionary
  , _info         :: TorrentInfo       -- a dictionary describing the torrent
  } deriving (Show, Eq)

data TorrentInfo = TorrentInfo
  { _name            :: Maybe String    -- the suggested name for the file/directory
  , _piecesLength    :: Length          -- the number of bytes in each piece of the torrent
  , _pieces          :: SHA1            -- SHA1 hashes of the pieces
  , _length          :: Length          -- length of the file in bytes (could also be files key for multi-file torrent)
  } deriving (Show, Eq)

$(makeLenses ''MetaInfo)
$(makeLenses ''TorrentInfo)
