module Network.Gazelle.Routes.Artist (
    artistRoute,
) where

import Network.Gazelle.Types

import Network.API.Builder
import Network.HTTP.Types.Method

import Data.Text (Text)

artistRoute :: ArtistID -> Route
artistRoute (ArtistID a) = Route {
    urlPieces = [],
    urlParams = [ "action" =. ("artist" :: Text)
                , "id"     =. a ],
    httpMethod = renderStdMethod GET
}
