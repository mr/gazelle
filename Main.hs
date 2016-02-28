{-# LANGUAGE OverloadedStrings #-}

import Network.Gazelle
import Control.Monad.IO.Class

username = "username"
password = "password"

main = do
    res <- runGazelle username password $ do
        tg <- getTorrentGroup $ TorrentGroupID 73103599
        liftIO $ print tg
        let artistId = ArtistID . eId . head . miArtists $ tgMusicInfo tg
        artist <- getArtist artistId
        liftIO $ print artist
    case res of
        Left e -> print e
        _ -> return ()
