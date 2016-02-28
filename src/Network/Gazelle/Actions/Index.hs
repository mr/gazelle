module Network.Gazelle.Actions.Index (
    getIndex
) where

import Network.Gazelle.Types
import Network.Gazelle.Routes

getIndex :: Monad m => GazelleT m Index
getIndex = receiveRoute $ indexRoute
