module Network.Gazelle.Types.Index (
    Index(..)
) where

import Data.Aeson
import Data.Text (Text)

data Index = Index {
    iUsername :: Text,
    iId :: Int
} deriving (Show)

instance FromJSON Index where
    parseJSON (Object o) = Index <$> o .: "username" <*> o .: "id"