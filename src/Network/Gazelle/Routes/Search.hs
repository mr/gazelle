module Network.Gazelle.Routes.Search (
    searchRoute
) where

import Data.Text (Text)

searchRoute :: Text -> Route
searchRoute t = Route {
    urlPieces = [],
    urlParams = [ "action" =. ("browse" :: Text)
                , "searchstr"     =. t ],
    httpMethod = renderStdMethod GET
}
