module Network.Gazelle.Actions.Search (
    defaultSearch
) where

defaultSearch :: Monad m => Text -> GazelleT m Page
defaultSearch = receiveRoute . torrentRoute
