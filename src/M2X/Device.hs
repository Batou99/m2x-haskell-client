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
import Data.Text (pack, unpack)

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

data LocationFilter 
    = LocationWithinCircle { point :: LocationPoint
                           , unit :: LocationUnit
                           , radius :: Double }
    | LocationWithinPolygon [LocationPoint]
    | NoLocation
    deriving (Show, Eq)

instance ToJSON LocationFilter where
    toJSON (LocationWithinCircle point unit radius) =
        object ["location" .=
            object ["within_circle" .= 
                object [ "center" .= toJSON point
                       , "radius" .= object [fromString (map toLower $ show unit) .= radius ]]]]
    toJSON (LocationWithinPolygon points) = 
        object ["location" .=
            object ["within_polygon" .= map toJSON points]]
    toJSON NoLocation = object ["location" .= ("none" :: String)]

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
data StreamValues = AllStreams Bool | SomeStreams [String] deriving (Show, Eq)
data Triggers     = SomeTriggers [String] | AllTriggers deriving (Show, Eq)
data CommandStatus = Pending | Rejected | Processing deriving (Show, Eq)
newtype FilterReqBody = FilterReqBody [Filter] deriving (Show, Eq)
type ID            = String
type Tag           = String
type Serial        = String
type AuthHeader    = Maybe String
data TagResult     = TagResult { name :: String, numberOfDevicesTagged :: Int } deriving (Show, Eq)
data TagResultList = TagResultList [TagResult] deriving (Show, Eq)

instance FromJSON TagResult where
    parseJSON (Object v) =
        let (k, _) = head (HML.toList v)
        in TagResult (unpack k) <$> v .: k
    parseJSON _ = fail "Invalid JSON data"

instance FromJSON TagResultList where
    parseJSON (Object v) = TagResultList <$> v .: "tags"

instance ToJSON FilterReqBody where
    toJSON (FilterReqBody filters) = mergeAeson (map toJSON filters)

instance ToHttpApiData SortBy where
    toUrlPiece = toUrlPiece . show

instance ToHttpApiData SortDir where
    toUrlPiece = toUrlPiece . show

instance ToHttpApiData [String] where
    toUrlPiece = toUrlPiece . intersperse ","

instance ToHttpApiData Triggers where
    toUrlPiece AllTriggers = "*"
    toUrlPiece (SomeTriggers values) = (toUrlPiece . intersperse ",") values

instance ToHttpApiData StreamValues where
    toUrlPiece (AllStreams b) = toUrlPiece . map toLower $ show b
    toUrlPiece (SomeStreams s) = toUrlPiece s

instance ToHttpApiData CommandStatus where
    toUrlPiece = toUrlPiece . show

instance ToHttpApiData Visibility where
    toUrlPiece = toUrlPiece . map toLower . show

instance ToHttpApiData Status where
    toUrlPiece = toUrlPiece . map toLower . show

type DeviceApi = "v2/devices/catalog" :> Get '[JSON] DevicePaginatedListing
            :<|> "v2/devices/catalog/search" :> QueryParam "name" String
                                             :> QueryParam "description" String
                                             :> QueryParam "page" Int
                                             :> QueryParam "limit" Int
                                             :> QueryParam "tags" [Tag]
                                             :> QueryParam "serial" [Serial]
                                             :> QueryParam "dir" SortDir
                                             :> QueryParam "sort" SortBy
                                             :> ReqBody '[JSON] FilterReqBody
                                             :> Get '[JSON] DevicePaginatedListing
            :<|> "v2/devices" :> Header "X-M2X-API" String 
                              :> QueryParam "include_stream_values" StreamValues
                              :> QueryParam "ids" [ID]
                              :> QueryParam "name" String
                              :> QueryParam "description" String
                              :> QueryParam "page" Int
                              :> QueryParam "limit" Int
                              :> QueryParam "tags" [Tag]
                              :> QueryParam "status" Status
                              :> QueryParam "visibility" Visibility
                              :> QueryParam "modified_since" UTCTime
                              :> QueryParam "unmodified_since" UTCTime
                              :> QueryParam "serial" [Serial]
                              :> QueryParam "collection" ID
                              :> QueryParam "distribution" ID
                              :> QueryParam "triggers" [String]
                              :> QueryParam "activated_triggers" Triggers
                              :> QueryParam "inactive_triggers" Triggers
                              :> QueryParam "enabled_triggers" Triggers
                              :> QueryParam "disabled_triggers" Triggers
                              :> QueryParam "command_status" CommandStatus
                              :> QueryParam "command_name" String
                              :> QueryParam "command_name" UTCTime
                              :> QueryParam "dir" SortDir
                              :> QueryParam "sort" SortBy
                              :> ReqBody '[JSON] FilterReqBody
                              :> Get '[JSON] DevicePaginatedListing
            :<|> "v2/devices/tags" :> Header "X-M2X-API" String 
                                   :> Get '[JSON] TagResultList

api :: Proxy DeviceApi
api = Proxy

data CatalogSearchParams = CatalogSearchParams { cspName :: Maybe String
                                               , cspDesc :: Maybe String
                                               , cspPage :: Maybe Int
                                               , cspLimit :: Maybe Int
                                               , cspTags :: Maybe [Tag]
                                               , cspSerial :: Maybe [Tag]
                                               , cspDir :: Maybe SortDir
                                               , cspSort :: Maybe SortBy
                                               , cspReqBody :: FilterReqBody}

data DevicesParams = DevicesParams { dpIncludeStreamValues :: Maybe StreamValues
                                   , dpIds :: Maybe [ID]
                                   , dpName :: Maybe String
                                   , dpDescription :: Maybe String
                                   , dpPage :: Maybe Int
                                   , dpLimit :: Maybe Int
                                   , dpTags :: Maybe [Tag]
                                   , dpStatus :: Maybe Status
                                   , dpVisibility :: Maybe Visibility
                                   , dpModifiedSince :: Maybe UTCTime
                                   , dpUnmodifiedSince :: Maybe UTCTime
                                   , dpSerial :: Maybe [Serial]
                                   , dpCollection :: Maybe ID
                                   , dpDistribution :: Maybe ID
                                   , dpTriggers :: Maybe [String]
                                   , dpActivatedTriggers :: Maybe Triggers
                                   , dpInactiveTriggers :: Maybe Triggers
                                   , dpEnabledTriggers :: Maybe Triggers
                                   , dpDisabledTriggers :: Maybe Triggers
                                   , dpCommandStatus :: Maybe CommandStatus
                                   , dpCommandName :: Maybe String
                                   , dpCommandSince :: Maybe UTCTime
                                   , dpDir :: Maybe SortDir
                                   , dpSort :: Maybe SortBy
                                   , dpReqBody :: FilterReqBody }

defaultCatalogSearchParams :: CatalogSearchParams
defaultCatalogSearchParams = CatalogSearchParams { cspName = Nothing
                                                 , cspDesc = Nothing
                                                 , cspPage = Just 1
                                                 , cspLimit = Just 100
                                                 , cspTags = Nothing
                                                 , cspSerial = Nothing
                                                 , cspDir = Just Asc
                                                 , cspSort = Just Name
                                                 , cspReqBody = FilterReqBody [] }

defaultDevicesParams :: DevicesParams
defaultDevicesParams = DevicesParams { dpIncludeStreamValues = Nothing 
                                     , dpIds = Nothing
                                     , dpName = Nothing
                                     , dpDescription = Nothing
                                     , dpPage = Just 1
                                     , dpLimit = Just 100
                                     , dpTags = Nothing
                                     , dpStatus = Nothing
                                     , dpVisibility = Nothing
                                     , dpModifiedSince = Nothing
                                     , dpUnmodifiedSince = Nothing
                                     , dpSerial = Nothing
                                     , dpCollection = Nothing
                                     , dpDistribution = Nothing
                                     , dpTriggers = Nothing
                                     , dpActivatedTriggers = Nothing
                                     , dpInactiveTriggers = Nothing
                                     , dpEnabledTriggers = Nothing
                                     , dpDisabledTriggers = Nothing
                                     , dpCommandStatus = Nothing
                                     , dpCommandName = Nothing
                                     , dpCommandSince = Nothing
                                     , dpDir = Just Asc
                                     , dpSort = Just Name
                                     , dpReqBody = FilterReqBody [] }

getCatalog :: ClientM DevicePaginatedListing
catalogSearch :: Maybe String -> Maybe String -> Maybe Int -> Maybe Int -> Maybe [Tag] -> Maybe [Serial] -> Maybe SortDir -> Maybe SortBy -> FilterReqBody -> ClientM DevicePaginatedListing
getDevices :: AuthHeader -> Maybe StreamValues -> Maybe [ID] -> Maybe String -> Maybe String -> Maybe Int -> Maybe Int -> Maybe [Tag] -> Maybe Status -> Maybe Visibility -> Maybe UTCTime -> Maybe UTCTime -> Maybe [Serial] -> Maybe ID -> Maybe ID -> Maybe [String] -> Maybe Triggers -> Maybe Triggers -> Maybe Triggers -> Maybe Triggers -> Maybe CommandStatus -> Maybe String -> Maybe UTCTime -> Maybe SortDir -> Maybe SortBy -> FilterReqBody -> ClientM DevicePaginatedListing
getCatalog :<|> catalogSearch :<|> getDevices :<|> getTags = client api

catalogSearchFromParams :: CatalogSearchParams -> ClientM DevicePaginatedListing
catalogSearchFromParams (CatalogSearchParams deviceName desc page limit tags serial dir sort reqBody) =
    catalogSearch deviceName desc page limit tags serial dir sort reqBody

getDevicesFromParams :: AuthHeader -> DevicesParams -> ClientM DevicePaginatedListing
getDevicesFromParams key (DevicesParams includeStreamValues ids deviceName description page limit tags status visibility modifiedSince unmodifiedSince serial collection distribution triggers activatedTriggers inactiveTriggers enabledTriggers disabledTriggers commandStatus commandName commandSince dir sort reqBody) =
    getDevices key includeStreamValues ids deviceName description page limit tags status visibility modifiedSince unmodifiedSince serial collection distribution triggers activatedTriggers inactiveTriggers enabledTriggers disabledTriggers commandStatus commandName commandSince dir sort reqBody
