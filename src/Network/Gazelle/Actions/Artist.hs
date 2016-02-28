module Network.Gazelle.Actions.Artist (
    getArtist
) where

import Network.Gazelle.Types
import Network.Gazelle.Routes

getArtist :: Monad m => ArtistID -> GazelleT m Artist
getArtist = receiveRoute . artistRoute
