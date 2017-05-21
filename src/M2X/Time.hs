{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module M2X.Time where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Servant.API
import Servant.Client

data Time = Time
  { seconds :: Integer,
    millis   :: Integer,
    iso8601 :: String
  } deriving (Show, Eq, Generic)

instance FromJSON Time

type API = "time" :> Get '[JSON] Time
type API_V2 = "v2" :> API

api :: Proxy API_V2
api = Proxy

time = client api
