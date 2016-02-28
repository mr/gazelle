module Network.Gazelle.Types.Index (
    Index(..)
) where

import Network.Gazelle.Types.Gazelle

import Data.Aeson
import Data.Text (Text)

import Network.API.Builder

data Index = Index {
    iUsername :: Text,
    iId :: Int
} deriving (Show)

instance FromJSON Index where
    parseJSON (Object o) = Index <$> o .: "username" <*> o .: "id"

instance Receivable Index where
    receive = useResponseFromJSON
