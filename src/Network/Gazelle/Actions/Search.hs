module Network.Gazelle.Actions.Search (
    defaultSearch
) where

import Network.Gazelle.Types
import Network.Gazelle.Routes

import Data.Text (Text)

defaultSearch :: Monad m => Text -> GazelleT m Page
defaultSearch = receiveRoute . searchRoute
