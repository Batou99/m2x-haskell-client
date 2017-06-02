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

type TIME_API = "time" :> Get '[JSON] Time
type TIME_API_V2 = "v2" :> TIME_API

api :: Proxy TIME_API_V2
api = Proxy

time :: ClientM Time
time = client api
