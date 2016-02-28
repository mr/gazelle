module Network.Gazelle.Routes.Index (
    indexRoute
) where

import Network.API.Builder
import Network.HTTP.Types.Method

import Data.Text (Text)

indexRoute :: Route
indexRoute = Route {
    urlPieces = [],
    urlParams = [ "action" =. ("index" :: Text) ],
    httpMethod = renderStdMethod GET
}
