module Network.Gazelle.Routes.Torrent (
    torrentRoute,
    torrentGroupRoute
) where

import Network.Gazelle.Types

import Data.Text (Text)

import Network.API.Builder
import Network.HTTP.Types.Method

torrentRoute :: TorrentID -> Route
torrentRoute (TorrentID t) = Route {
    urlPieces = [],
    urlParams = [ "action" =. ("torrent" :: Text)
                , "id"     =. t ],
    httpMethod = renderStdMethod GET
}

torrentGroupRoute :: TorrentGroupID -> Route
torrentGroupRoute (TorrentGroupID t) = Route {
    urlPieces = [],
    urlParams = [ "action" =. ("torrentgroup" :: Text)
                , "id"     =. t ],
    httpMethod = renderStdMethod GET
}
