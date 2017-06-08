{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

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
import Data.String (fromString)
import qualified Data.HashMap.Lazy as HML

mergeAeson :: [Value] -> Value
mergeAeson = Object . HML.unions . map (\(Object x) -> x)

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

data MultipleFilterOp 
    = StreamGt Double
    | StreamGte Double
    | StreamLt Double
    | StreamLte Double
    deriving (Show, Eq, Generic)

instance ToJSON MultipleFilterOp where
    toJSON = genericToJSON
      defaultOptions { constructorTagModifier = drop 6 . map toLower
                     , sumEncoding = ObjectWithSingleField }

data SingleFilterOp
    = StreamEq Double
    | StreamMatch String
    deriving (Show, Eq, Generic)

instance ToJSON SingleFilterOp where
    toJSON = genericToJSON $ defaultOptions { constructorTagModifier = drop 6 . map toLower,
                                              sumEncoding = ObjectWithSingleField }

data StreamMultipleFilter = StreamMultipleFilter { fieldName :: String, ops :: [MultipleFilterOp]}
    deriving (Eq, Show)

instance ToJSON StreamMultipleFilter where
    toJSON (StreamMultipleFilter fieldName ops) =
        object [fromString fieldName .= mergeAeson (map toJSON ops)]

data StreamSingleFilter = StreamSingleFilter { fieldName :: String, ops :: SingleFilterOp }
    deriving (Eq, Show)

instance ToJSON StreamSingleFilter where
    toJSON (StreamSingleFilter fieldName op) = object [fromString fieldName .= toJSON op]

data StreamFilter = StreamFilter [StreamMultipleFilter] [StreamSingleFilter]
    deriving (Eq, Show)

instance ToJSON StreamFilter where
    toJSON (StreamFilter multipleFilters singleFilters) = 
        let aesonMaps = map toJSON multipleFilters ++ map toJSON singleFilters
        in object ["streams" .= mergeAeson aesonMaps]

data MetadataSingleFilter = MetadataSingleFilter { fieldName :: String, value :: String }
    deriving (Eq, Show)

instance ToJSON MetadataSingleFilter where
    toJSON (MetadataSingleFilter fieldName value) =
        object [fromString fieldName .= object ["match" .= value]]

newtype MetadataFilter = MetadataFilter [MetadataSingleFilter] deriving (Show, Eq, Generic)

instance ToJSON MetadataFilter where
    toJSON (MetadataFilter filters) = object ["metadata" .= mergeAeson (map toJSON filters)]

data LocationUnit = Mi | Miles | Km deriving (Show, Eq)

data LocationPoint = LocationPoint { latitude :: Double, longitude :: Double }
    deriving (Show, Eq, Generic, ToJSON)

data LocationWithinCircle
    = LocationWithinCircle { point :: LocationPoint
                           , unit :: LocationUnit
                           , radius :: Double }
    deriving (Show, Eq)

instance ToJSON LocationWithinCircle where
    toJSON (LocationWithinCircle point unit radius) =
        object ["within_circle" .= 
            object [ "center" .= toJSON point
                   , "radius" .= object [fromString (map toLower $ show unit) .= radius ]]]

newtype LocationWithinPolygon = LocationWithinPolygon [LocationPoint]
  deriving (Show, Eq, Generic)

data NoLocation = NoLocation deriving (Show, Eq, Generic)

instance ToJSON NoLocation where
    toJSON NoLocation = "none"

instance ToJSON LocationWithinPolygon where
    toJSON (LocationWithinPolygon points) = object ["within_polygon" .= map toJSON points]

data LocationFilter 
    = LocationFilterWithinCircle LocationWithinCircle
    | LocationFilterWithinPolygon LocationWithinPolygon
    | LocationNone NoLocation
    deriving (Show, Eq, Generic)

instance ToJSON LocationFilter where
    toJSON = genericToJSON
      defaultOptions { constructorTagModifier = const "location" 
                     , sumEncoding = ObjectWithSingleField }

data Filter 
    = Stream StreamFilter 
    | Location LocationFilter 
    | Metadata MetadataFilter
    deriving (Show, Eq, Generic, ToJSON)

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
newtype FilterReqBody = FilterReqBody [Filter] deriving (Show, Eq)

instance ToJSON FilterReqBody where
    toJSON (FilterReqBody filters) = mergeAeson (map toJSON filters)

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
                                             :> ReqBody '[JSON] FilterReqBody
                                             :> Get '[JSON] DevicePaginatedListing

api :: Proxy DeviceApi
api = Proxy

getCatalog :: AuthParam -> ClientM DevicePaginatedListing
catalogSearch :: AuthParam -> NameParam -> DescParam -> PageParam -> LimitParam -> TagsParam -> SerialParam -> DirParam -> SortParam -> FilterReqBody -> ClientM DevicePaginatedListing
getCatalog :<|> catalogSearch = client api
