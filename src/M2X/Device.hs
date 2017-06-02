{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module M2X.Device where

import Network.URL
import Data.Aeson
import Data.Aeson.Types
import Data.Proxy
import Data.Char
import Data.List (intersperse)
import GHC.Generics
import Servant.API
import Servant.Client
import Data.Time (UTCTime)
import Text.Read (readMaybe)
import Data.Foldable (asum)
import Control.Applicative

type Metadata = [(String, String)]

data Visibility = Public | Private deriving (Show, Eq, Generic)
instance FromJSON Visibility where
  parseJSON = genericParseJSON $ defaultOptions {constructorTagModifier = map toLower}

data Status = Enabled | Disabled deriving (Show, Eq, Generic)
instance FromJSON Status where
  parseJSON = genericParseJSON $ defaultOptions {constructorTagModifier = map toLower}

data Waypoint = Waypoint { name :: Maybe String
                         , latitude :: Double
                         , longitude :: Double
                         , elevation :: Maybe Double
                         , timestamp :: UTCTime
                         } deriving (Show, Eq)

instance FromJSON Waypoint where
    parseJSON = withObject "waypoint" $ \o -> do
        name      <- o .:? "name"
        latitude  <- o .: "latitude"
        longitude <- o .: "longitude"
        timestamp <- o .: "timestamp"
        elevation <- asum [
          o .: "elevation",
          do s <- o .: "elevation"
             case readMaybe s of
               Nothing -> fail "not a number"
               Just x  -> return x
                          ]
        return Waypoint{..}

data Device = Device { id :: String
                     , url :: String
                     , name :: String
                     , description :: Maybe String
                     , visibility :: Visibility
                     , status :: Status
                     , collections :: [String]
                     , serial :: Maybe String
                     , tags :: [String]
                     , location :: Maybe Waypoint
                     , lastActivity :: UTCTime
                     , created :: UTCTime
                     , updated :: UTCTime
                     } deriving (Show, Eq, Generic)

instance FromJSON Device where
    parseJSON = withObject "device" $ \o -> do
        id           <- o .: "id"
        url          <- o .: "url"
        name         <- o .: "name"
        description  <- o .:? "description"
        visibility   <- o .: "visibility"
        status       <- o .: "status"
        collections  <- o .: "collections"
        serial       <- o .:? "serial"
        tags         <- o .: "tags"
        location     <- optional (o .: "location")
        lastActivity <- o .: "last_activity"
        created      <- o .: "created"
        updated      <- o .: "updated"
        return Device{..}

data DevicePaginatedListing = DevicePaginatedListing { devices :: [Device]
                                                     , total :: Int
                                                     , pages :: Int
                                                     , limit :: Int
                                                     , currentPage :: Int
                                                     } deriving (Show, Eq, Generic)

instance FromJSON DevicePaginatedListing where
    parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = camelTo2 '_'}

data Collection = Collection { id :: String
                             , parent :: Maybe String
                             , name :: String
                             , description :: String
                             , devices :: Int
                             , collections :: Int
                             , tags :: [String]
                             , metadata :: Maybe Metadata
                             , key :: String
                             , created :: UTCTime
                             , updated :: UTCTime
                             } deriving (Show, Eq)

data DeviceList = DeviceList { total :: Int
                             , registered :: Int
                             , unregistered :: Int
                             } deriving (Show, Eq)

data Distribution = Distribution { id :: String
                                 , name :: String
                                 , description :: String
                                 , visibility :: Visibility
                                 , status :: Status
                                 , url :: URL
                                 , key :: String
                                 , created :: UTCTime
                                 , updated :: UTCTime
                                 , devices :: DeviceList
                                 } deriving (Show, Eq)


data SortBy        = Created | Name deriving (Show, Eq)
data SortDir       = Asc | Desc deriving (Show, Eq)
type Tag           = String
type AuthParam     = Maybe String
type NameParam     = Maybe String
type DescParam     = Maybe String
type PageParam     = Maybe Int
type LimitParam    = Maybe Int
type TagsParam     = Maybe [Tag]
type SerialParam   = Maybe [Tag]
type DirParam      = Maybe SortDir
type SortParam     = Maybe SortBy

instance ToHttpApiData SortBy where
    toUrlPiece = toUrlPiece . show

instance ToHttpApiData SortDir where
    toUrlPiece = toUrlPiece . show

instance ToHttpApiData [Tag] where
    toUrlPiece = toUrlPiece . intersperse ","

type DeviceApi = "v2/devices/catalog" :> Header "X-M2X-API" String 
                                      :> Get '[JSON] DevicePaginatedListing
            :<|> "v2/devices/catalog/search" :> Header "X-M2X-API" String 
                                             :> QueryParam "name" String
                                             :> QueryParam "description" String
                                             :> QueryParam "page" Int
                                             :> QueryParam "limit" Int
                                             :> QueryParam "tags" [Tag]
                                             :> QueryParam "serial" [Tag]
                                             :> QueryParam "dir" SortDir
                                             :> QueryParam "sort" SortBy
                                             :> Get '[JSON] DevicePaginatedListing

api :: Proxy DeviceApi
api = Proxy

getCatalog :: AuthParam -> ClientM DevicePaginatedListing
catalogSearch :: AuthParam -> NameParam -> DescParam -> PageParam -> LimitParam -> TagsParam -> SerialParam -> DirParam -> SortParam -> ClientM DevicePaginatedListing
getCatalog :<|> catalogSearch = client api
