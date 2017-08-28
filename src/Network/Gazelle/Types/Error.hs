module Network.Gazelle.Types.Error (
    GazelleError(..)
) where

import Data.Aeson

import Data.Text (Text)

data GazelleError = GenericGazelleError
    deriving Show

instance FromJSON GazelleError where
    parseJSON = withObject "GazelleError" $ \o -> do
        stat <- o .: "status"
        let textStat = stat :: Text
        if textStat == "failure"
            then return GenericGazelleError
            else fail "Not an error"
