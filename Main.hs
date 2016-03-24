{-# LANGUAGE OverloadedStrings #-}

import Network.Gazelle
import Control.Monad.IO.Class

username = "username"
password = "password"

main = do
    res <- runGazelle username password $ \cj ->
        defaultSearch "Death Grips" >>= liftIO . print
    case res of
        Left e -> print e
        _ -> return ()
