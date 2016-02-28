module Network.Gazelle.Login (
    login,
    loginRoute
) where

import Data.Text (Text)

import Network.API.Builder
import Network.HTTP.Client
import Network.HTTP.Types.Method

import Network.Gazelle.Types

loginRoute :: Text -> Text -> Route
loginRoute user pass = Route {
    urlPieces = [],
    urlParams = [ "username" =. user
                , "password" =. pass ],
    httpMethod = renderStdMethod POST
}

login :: Monad m => Text -> Text -> GazelleT m CookieJar
login user pass = receiveRoute $ loginRoute user pass

instance Receivable CookieJar where
    receive = return . responseCookieJar

