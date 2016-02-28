module Network.Gazelle.Types.Torrent (
    Entry(..),
    MusicInfo(..),
    Torrent(..),
    TorrentID(..),
    TorrentGroup(..),
    TorrentGroupID(..),
    TorrentAndGroup(..),
    TorrentGroupAndChildren(..)
) where

import Network.Gazelle.Types.Gazelle

import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import Data.Scientific

import Network.API.Builder

newtype TorrentID = TorrentID Integer
    deriving Show

instance FromJSON TorrentID where
    parseJSON = withScientific "TorrentID" $
        return . TorrentID . round . toRealFloat

data Torrent = Torrent {
    tId :: TorrentID,
    tMedia :: Text,
    tFormat :: Text,
    tEncoding :: Text,
    tRemastered :: Bool,
    tRemasterYear :: Integer,
    tRemasterTitle :: Text,
    tRemasterRecordLabel :: Text,
    tRemasterCatalogueNumber :: Text,
    tScene :: Bool,
    tHasLog :: Bool,
    tHasCue :: Bool,
    tLogScore :: Integer,
    tFileCount :: Integer,
    tSize :: Integer,
    tSeeders :: Integer,
    tLeechers :: Integer,
    tSnatched :: Integer,
    tFreeTorrent :: Bool,
    tTime :: Text,
    tDescription :: Text,
    tFileList :: Text,
    tFilePath :: Text,
    tUserId :: Integer
} deriving Show

instance FromJSON Torrent where
    parseJSON  = withObject "Torrent" $ \o -> Torrent <$>
        o .: "id" <*>
        o .: "media" <*>
        o .: "format" <*>
        o .: "encoding" <*>
        o .: "remastered" <*>
        o .: "remasterYear" <*>
        o .: "remasterTitle" <*>
        o .: "remasterRecordLabel" <*>
        o .: "remasterCatalogueNumber" <*>
        o .: "scene" <*>
        o .: "hasLog" <*>
        o .: "hasCue" <*>
        o .: "logScore" <*>
        o .: "fileCount" <*>
        o .: "size" <*>
        o .: "seeders" <*>
        o .: "leechers" <*>
        o .: "snatched" <*>
        o .: "freeTorrent" <*>
        o .: "time" <*>
        o .: "description" <*>
        o .: "fileList" <*>
        o .: "filePath" <*>
        o .: "userId"

instance Receivable Torrent where
    receive = useResponseFromJSON

newtype TorrentGroupID = TorrentGroupID Integer
    deriving Show

instance FromJSON TorrentGroupID where
    parseJSON = withScientific "TorrentGroupID" $
        return . TorrentGroupID . round . toRealFloat

data MusicInfo = MusicInfo {
    miComposers :: [Entry],
    miDj :: [Entry],
    miArtists :: [Entry],
    miWith :: [Entry],
    miConductor :: [Entry],
    miRemixedBy :: [Entry],
    miProducer :: [Entry]
} deriving Show

instance FromJSON MusicInfo where
    parseJSON = withObject "MusicInfo" $ \o -> MusicInfo <$>
        o .: "composers" <*>
        o .: "dj" <*>
        o .: "artists" <*>
        o .: "with" <*>
        o .: "conductor" <*>
        o .: "remixedBy" <*>
        o .: "producer"

data Entry = Entry {
    eId :: Integer,
    eName :: Text
} deriving Show

instance FromJSON Entry where
    parseJSON = withObject "Entry" $ \o -> Entry <$>
        o .: "id" <*>
        o .: "name"

data TorrentGroup = TorrentGroup {
    tgId :: TorrentGroupID,
    tgName :: Text,
    tgYear :: Integer,
    tgRecordLabel :: Text,
    tgCatalogueNumber :: Text,
    tgReleaseType :: Integer,
    tgCategoryId :: Integer,
    tgCategoryName :: Text,
    tgTime :: Text,
    tgVanityHouse :: Bool,
    tgMusicInfo :: MusicInfo
} deriving Show

instance FromJSON TorrentGroup where
    parseJSON = withObject "TorrentGroup" $ \o -> TorrentGroup <$>
        o .: "id" <*>
        o .: "name" <*>
        o .: "year" <*>
        o .: "recordLabel" <*>
        o .: "catalogueNumber" <*>
        o .: "releaseType" <*>
        o .: "categoryId" <*>
        o .: "categoryName" <*>
        o .: "time" <*>
        o .: "vanityHouse" <*>
        o .: "musicInfo"

instance Receivable TorrentGroup where
    receive = useResponseFromJSON

data TorrentAndGroup = TorrentAndGroup {
    tagGroup :: TorrentGroup,
    tagTorrent :: Torrent
} deriving Show

instance FromJSON TorrentAndGroup where
    parseJSON = withObject "TorrentAndGroup" $ \o -> TorrentAndGroup <$>
        o .: "group" <*>
        o .: "torrent"

instance Receivable TorrentAndGroup where
    receive = useResponseFromJSON

data TorrentGroupAndChildren = TorrentGroupAndChildren {
    tgcGroup :: TorrentGroup,
    tgcChildren :: [Torrent]
 } deriving Show

instance FromJSON TorrentGroupAndChildren where
    parseJSON = withObject "TorrentGroupAndChildren" $ \o -> TorrentGroupAndChildren <$>
        o .: "group" <*>
        o .: "torrents"

instance Receivable TorrentGroupAndChildren where
    receive = useResponseFromJSON
