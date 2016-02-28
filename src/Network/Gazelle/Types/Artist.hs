module Network.Gazelle.Types.Artist (
    Artist(..),
    ArtistID(..),
    Tag(..),
    ArtistStatistics(..)
) where

import Network.Gazelle.Types.Gazelle

import Network.API.Builder

import Data.Aeson
import Data.Scientific

import Data.Text (Text)

newtype ArtistID = ArtistID Integer
    deriving Show

instance FromJSON ArtistID where
    parseJSON = withScientific "ArtistID" $
        return . ArtistID . round . toRealFloat

data Tag = Tag {
    tagName :: Text,
    tagCount :: Integer
 } deriving Show

instance FromJSON Tag where
    parseJSON = withObject "Tag" $ \o -> Tag <$>
        o .: "name" <*>
        o .: "count"

data ArtistStatistics = ArtistStatistics {
    asNumGroups :: Integer,
    asNumTorrents :: Integer,
    asNumSeeders :: Integer,
    asNumLeechers :: Integer,
    asNumSnatches :: Integer
} deriving Show

instance FromJSON ArtistStatistics where
    parseJSON = withObject "ArtistStatistics" $ \o -> ArtistStatistics <$>
        o .: "numGroups" <*>
        o .: "numTorrents" <*>
        o .: "numSeeders" <*>
        o .: "numLeechers" <*>
        o .: "numSnatches"

newtype SimilarId = SimilarId Integer
    deriving Show

instance FromJSON SimilarId where
    parseJSON = withScientific "SimilarId" $
        return . SimilarId . round . toRealFloat

data ArtistEntry = ArtistEntry {
    aeId :: Integer,
    aeName :: Text,
    aeScore :: Integer,
    aeSimilarId :: SimilarId
} deriving Show

instance FromJSON ArtistEntry where
    parseJSON = withObject "ArtistEntry" $ \o -> ArtistEntry <$>
        o .: "artistId" <*>
        o .: "name" <*>
        o .: "score" <*>
        o .: "similarId"

data Artist = Artist {
    aId :: ArtistID,
    aName :: Text,
    aNotificationsEnabled :: Bool,
    aHasBookmarked :: Bool,
    aImage :: Text,
    aBody :: Text,
    aVanityHouse :: Bool,
    aTags :: [Tag],
    aSimilarArtists :: [ArtistEntry],
    aStatistics :: ArtistStatistics
} deriving Show

instance FromJSON Artist where
    parseJSON = withObject "Artist" $ \o -> Artist <$>
        o .: "id" <*>
        o .: "name" <*>
        o .: "notificationsEnabled" <*>
        o .: "hasBookmarked" <*>
        o .: "image" <*>
        o .: "body" <*>
        o .: "vanityHouse" <*>
        o .: "tags" <*>
        o .: "similarArtists" <*>
        o .: "statistics"

instance Receivable Artist where
    receive = useResponseFromJSON
