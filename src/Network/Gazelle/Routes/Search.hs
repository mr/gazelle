module Network.Gazelle.Routes.Search (
    searchRoute
) where

import Data.Text (Text)

import Network.API.Builder
import Network.HTTP.Types.Method

searchRoute :: Text -> Route
searchRoute t = Route {
    urlPieces = [],
    urlParams = [ "action" =. ("browse" :: Text)
                , "searchstr"     =. t ],
    httpMethod = renderStdMethod GET
}
