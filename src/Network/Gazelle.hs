module Network.Gazelle (
    whatCD,
    torrentRoute,
    runGazelle,
    Torrent(..),
    TorrentID(..),
    TorrentGroup(..),
    TorrentGroupID(..),
    MusicInfo(..),
    Entry(..),
    Artist(..),
    ArtistID(..),
    getArtist,
    Tag(..),
    ArtistStatistics(..),
    getTorrent,
    getTorrentAndGroup,
    getTorrentGroup,
    getTorrentGroupAndChildren,
    getIndex
) where

import Network.Gazelle.Actions
import Network.Gazelle.Login
import Network.Gazelle.Routes
import Network.Gazelle.Types

import Control.Monad.IO.Class
import Control.Monad.Trans.Free
import Control.Monad

import Data.Text (Text)

import Network.API.Builder as API
import Network.HTTP.Client.Internal (expose, cookie_name, CookieJar(..))
import Network.HTTP.Client hiding (Response)
import Network.HTTP.Client.TLS

runGazelle :: MonadIO m => Text -> Text -> GazelleT m a -> m (Either (APIError GazelleError) a)
runGazelle user pass = runGazelleWith (GazelleOptions user pass whatCD whatCDLogin tlsManagerSettings)

runGazelleWith :: MonadIO m => GazelleOptions -> GazelleT m a -> m (Either (APIError GazelleError) a)
runGazelleWith (GazelleOptions user pass b lb ms) gazelle = do
    manager <- liftIO $ newManager ms
    allLoginCreds <- fmap (fmap Just) $ interpretIO (GazelleState manager Nothing lb) $ login user pass
    let loginCreds = fmap sessionCJ <$> allLoginCreds
    case loginCreds of
        Left err -> return $ Left err
        Right cookieJar -> let bb = b { _customizeRequest = \req -> req { cookieJar = cookieJar } } in
            interpretIO (GazelleState manager cookieJar bb) gazelle
    

interpretIO :: MonadIO m => GazelleState -> GazelleT m a -> m (Either (APIError GazelleError) a)
interpretIO gstate (GazelleT g) = runFreeT g >>= \case
    Pure x -> return $ Right x
    Free (FailWith x) -> return $ Left x
    Free (RunRoute route n) -> interpretIO gstate $ GazelleT $ wrap $ ReceiveRoute route (n . unwrapJSON)
    Free (ReceiveRoute route n) -> handleReceive route gstate >>= \case
        Left err -> return $ Left err
        Right x -> interpretIO gstate $ GazelleT $ n x

handleReceive :: (MonadIO m, Receivable a) => Route -> GazelleState -> m (Either (APIError GazelleError) a)
handleReceive r s = do
    let cj = creds s
        b = builder s
        m = connMgr s
    (res, _, _) <- runAPI b m () $ API.runRoute r
    return res

whatCD :: Builder
whatCD = basicBuilder "WhatCD API" "http://what.cd/ajax.php"

sessionCJ :: CookieJar -> CookieJar
sessionCJ = CJ . filter ((== "session") . cookie_name) . expose

whatCDLogin :: Builder
whatCDLogin = basicBuilder "WhatCD API Login" "http://what.cd/login.php"
