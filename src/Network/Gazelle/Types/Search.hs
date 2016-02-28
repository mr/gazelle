module Network.Gazelle.Types.Search (
    Page(..),
    SearchResult(..),
    TRArtist(..),
    TorrentResult(..)
) where

import Data.Aeson
import Data.Text (Text)

import Network.API.Builder
import Network.Gazelle.Types.Gazelle

data Page = Page {
    pCurrentPage :: Integer,
    pPages :: Integer,
    pResults :: [SearchResult]
} deriving Show

instance FromJSON Page where
    parseJSON = withObject "Page" $ \o -> Page <$>
        o .: "currentPage" <*>
        o .: "pages" <*>
        o .: "results"

instance Receivable Page where
    receive = useResponseFromJSON

data SearchResult = SearchResult {
    srGroupId :: Integer,
    srGroupName :: Text,
    srArtist :: Text,
    srTags :: [Text],
    srBookmarked :: Bool,
    srVanityHouse :: Bool,
    srGroupYear :: Integer,
    srReleaseType :: Text,
    srGroupTime :: Text,
    srMaxSize :: Integer,
    srTotalSnatched :: Integer,
    srTotalSeeders :: Integer,
    srTotalLeechers :: Integer
} deriving Show

instance FromJSON SearchResult where
    parseJSON = withObject "SearchResult" $ \o -> SearchResult <$>
        o .: "groupId" <*>
        o .: "groupName" <*>
        o .: "artist" <*>
        o .: "tags" <*>
        o .: "bookmarked" <*>
        o .: "vanityHouse" <*>
        o .: "groupYear" <*>
        o .: "releaseType" <*>
        o .: "groupTime" <*>
        o .: "maxSize" <*>
        o .: "totalSnatched" <*>
        o .: "totalSeeders" <*>
        o .: "totalLeechers"

data TRArtist = TRArtist {
    traId :: Integer,
    traName :: Text,
    traAliasId :: Integer
} deriving Show

instance FromJSON TRArtist where
    parseJSON = withObject "TRArtist" $ \o -> TRArtist <$>
        o .: "id" <*>
        o .: "name" <*>
        o .: "aliasId"

data TorrentResult = TorrentResult {
    trId :: Integer,
    trEditionId :: Integer,
    trArtists :: [TRArtist],
    trRemastered :: Bool,
    trRemasterYear :: Integer,
    trRemasterCatalogueNumber :: Text,
    trRemasterTitle :: Text,
    trMedia :: Text,
    trEncoding :: Text,
    trFormat :: Text,
    trHasLog :: Bool,
    trLogScore :: Integer,
    trHasCue :: Bool,
    trScene :: Bool,
    trVanityHouse :: Bool,
    trFileCount :: Integer,
    trTime :: Text,
    trSize :: Integer,
    trSnatches :: Integer,
    trSeeders :: Integer,
    trLeechers :: Integer,
    trIsFreeleech :: Bool,
    trIsNeutralLeech :: Bool,
    trIsPersonalFreeleech :: Bool,
    trCanUseToken :: Bool
} deriving Show

instance FromJSON TorrentResult where
    parseJSON = withObject "TorrentResult" $ \o -> TorrentResult <$>
        o .: "id" <*>
        o .: "editionId" <*>
        o .: "artists" <*>
        o .: "remastered" <*>
        o .: "remasterYear" <*>
        o .: "remasterCatalogueNumber" <*>
        o .: "remasterTitle" <*>
        o .: "media" <*>
        o .: "encoding" <*>
        o .: "format" <*>
        o .: "hasLog" <*>
        o .: "logScore" <*>
        o .: "hasCue" <*>
        o .: "scene" <*>
        o .: "vanityHouse" <*>
        o .: "fileCount" <*>
        o .: "time" <*>
        o .: "size" <*>
        o .: "snatches" <*>
        o .: "seeders" <*>
        o .: "leechers" <*>
        o .: "isFreeleech" <*>
        o .: "isNeutralLeech" <*>
        o .: "isPersonalFreeleech" <*>
        o .: "canUseToken"
