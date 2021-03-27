-- | 

{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Torrent
  ( FLength
  , MetaInfo(..)
  , announce
  , infoHash
  , info
  , TorrentInfo(..)
  , name
  , piecesLength
  , pieces
  , length
  ) where

import Prelude hiding (id, length)

--import Lens.Micro.GHC
import Lens.Micro.TH

import Piece (PLength, SHA1(..))

---------------

type FLength = Int

data MetaInfo = Meta
  { _announce     :: String            -- the URL of the tracker
  , _infoHash     :: SHA1              -- a hash of info dictionary
  , _info         :: TorrentInfo       -- a dictionary describing the torrent
  } deriving (Show, Eq)

data TorrentInfo = TorrentInfo
  { _name            :: Maybe String    -- the suggested name for the file/directory
  , _piecesLength    :: PLength         -- the number of bytes in each piece of the torrent
  , _pieces          :: SHA1            -- SHA1 hashes of the pieces
  , _length          :: FLength         -- length of the file in bytes (could also be files key for multi-file torrent)
  } deriving (Show, Eq)

$(makeLenses ''MetaInfo)
$(makeLenses ''TorrentInfo)
