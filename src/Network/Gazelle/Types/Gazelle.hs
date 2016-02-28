module Network.Gazelle.Types.Gazelle (
    GazelleOptions(..),
    GazelleResponse(..),
    GazelleState(..),
    GazelleF(..),
    GazelleT(..),
    failWith,
    runRoute,
    receiveRoute,
    useResponseFromJSON
) where

import Network.Gazelle.Types.Error

import Control.Monad.Trans
import Control.Monad.Trans.Free

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)

import Network.API.Builder hiding (runRoute)
import Network.HTTP.Client

data GazelleOptions = GazelleOptions {
    username :: Text,
    password :: Text,
    rootApiBuilder :: Builder,
    loginBuilder :: Builder,
    managerSettings :: ManagerSettings
}

data GazelleState = GazelleState {
    connMgr :: Manager,
    creds :: Maybe CookieJar,
    builder :: Builder
}

instance Show GazelleState where
    show g = show $ creds g

data GazelleF a where
    FailWith :: APIError GazelleError -> GazelleF a
    ReceiveRoute :: Receivable b => Route -> (b -> a) -> GazelleF a
    RunRoute :: FromJSON b => Route -> (b -> a) -> GazelleF a

instance Functor GazelleF where
    fmap _ (FailWith x) = FailWith x
    fmap f (ReceiveRoute r x) = ReceiveRoute r (fmap f x)
    fmap f (RunRoute r x) = RunRoute r (fmap f x)

newtype GazelleT m a = GazelleT (FreeT GazelleF m a)
    deriving (Functor, Applicative, Monad)

instance MonadTrans GazelleT where
    lift = GazelleT . lift

instance MonadIO m => MonadIO (GazelleT m) where
    liftIO = GazelleT . liftIO

failWith :: Monad m => APIError GazelleError -> GazelleT m a
failWith = GazelleT . liftF . FailWith

runRoute :: (FromJSON a, Monad m) => Route -> GazelleT m a
runRoute r = GazelleT $ liftF $ RunRoute r id

receiveRoute :: (Receivable a, Monad m) => Route -> GazelleT m a
receiveRoute r = GazelleT $ liftF $ ReceiveRoute r id

data GazelleResponse a = GazelleResponse {
    status :: String,
    response :: a
} deriving (Show)

instance (FromJSON a) => FromJSON (GazelleResponse a) where
    parseJSON (Object o) = do
        stat <- o .: "status"
        if stat == "success"
            then GazelleResponse stat <$> o .: "response"
            else fail "Unsuccessful response"

useResponseFromJSON :: (FromJSON a, ErrorReceivable e) => Response ByteString -> Either (APIError e) a
useResponseFromJSON resp =
    case eitherDecode $ responseBody resp of
        Left err ->
            Left $ case receiveError resp of
                Just x -> APIError x
                Nothing -> ParseError err
        Right x -> return $ response x
