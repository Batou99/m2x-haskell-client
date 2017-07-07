{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module M2X.DeviceAPI where

import M2X.DeviceTypes
import Data.Proxy
import Servant.API
import Servant.Client
import Data.Time (UTCTime)

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
            {-:<|> "v2/devices" :> Header "X-M2X-API"-}
                              {-:> QueryParam "name" String-}
                              {-:> QueryParam "description" String-}
                              {-:> QueryParam "visibility" Visibility-}
                              {-:> QueryParam "tags" [Tag]-}
                              {-:> QueryParam "metadata" Metadata-}
                              {-:> QueryParam "base_device" ID-}
                              {-:> QueryParam "serial" String-}

api :: Proxy DeviceApi
api = Proxy

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

getCatalog 
  :<|> catalogSearch 
  :<|> getDevices 
  :<|> getTags 
    = client api

catalogSearchFromParams :: CatalogSearchParams -> ClientM DevicePaginatedListing
catalogSearchFromParams (CatalogSearchParams deviceName desc page limit tags serial dir sort reqBody) =
    catalogSearch deviceName desc page limit tags serial dir sort reqBody

getDevicesFromParams :: AuthHeader -> DevicesParams -> ClientM DevicePaginatedListing
getDevicesFromParams key (DevicesParams includeStreamValues ids deviceName description page limit tags status visibility modifiedSince unmodifiedSince serial collection distribution triggers activatedTriggers inactiveTriggers enabledTriggers disabledTriggers commandStatus commandName commandSince dir sort reqBody) =
    getDevices key includeStreamValues ids deviceName description page limit tags status visibility modifiedSince unmodifiedSince serial collection distribution triggers activatedTriggers inactiveTriggers enabledTriggers disabledTriggers commandStatus commandName commandSince dir sort reqBody
