{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Network.Gazelle (
    module  Network.Gazelle.Types
) where

import Network.Gazelle.Types
import Data.Text as T
import Servant.API
import Servant.Client
import Servant.Common.Req

import Data.Typeable
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)

data QueryParamConst (paramSym :: Symbol) (valueSym :: Symbol)
    deriving (Typeable)

instance (KnownSymbol paramSym, KnownSymbol valueSym, HasClient api) => HasClient (QueryParamConst paramSym valueSym :> api) where
    type Client (QueryParamConst paramSym valueSym :> api) = Client api

    clientWithRoute Proxy req =
        let pname = symbolVal (Proxy :: Proxy paramSym)
            vname = symbolVal (Proxy :: Proxy valueSym)
        in clientWithRoute (Proxy :: Proxy api)
            (appendToQueryString (T.pack pname) (Just $ T.pack vname) req)

data GazelleResponse a = GazelleResponse {
    grStatus :: String,
    grResponse :: a
}

type GazelleAPI = "ajax.php" :> QueryParam "action" String