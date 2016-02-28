module Network.Gazelle.Types.Error (
    GazelleError(..)
) where

import Data.Aeson

import Network.API.Builder

import Data.Text (Text)

data GazelleError = GenericGazelleError
    deriving Show

instance ErrorReceivable GazelleError where
    receiveError = useErrorFromJSON

instance FromJSON GazelleError where
    parseJSON = withObject "GazelleError" $ \o -> do
        stat <- o .: "status"
        let textStat = stat :: Text
        if textStat == "failure"
            then return GenericGazelleError
            else fail "Not an error"
