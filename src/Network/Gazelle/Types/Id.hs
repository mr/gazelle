module Network.Gazelle.Types.Id (
    ArtistID(..),
    SimilarId(..),
    TorrentID(..),
    TorrentGroupID(..)
) where

import Data.Aeson

newtype ArtistID = ArtistID Integer
    deriving Show

instance FromJSON ArtistID where
    parseJSON v = ArtistID <$> parseJSON v

newtype SimilarId = SimilarId Integer
    deriving Show

instance FromJSON SimilarId where
    parseJSON v = SimilarId <$> parseJSON v

newtype TorrentID = TorrentID Integer
    deriving Show

instance FromJSON TorrentID where
    parseJSON v = TorrentID <$> parseJSON v

newtype TorrentGroupID = TorrentGroupID Integer
    deriving Show

instance FromJSON TorrentGroupID where
    parseJSON v = TorrentGroupID <$> parseJSON v
