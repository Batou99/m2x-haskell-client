{-# LANGUAGE DuplicateRecordFields #-}

module M2X.Types where

import Data.HexString
import Data.DateTime
import Network.URL
import Data.IP

data Visibility = Public | Private deriving (Show, Eq)
data Status = Enabled | Disabled deriving (Show, Eq)
data Waypoint = Waypoint { name :: String
                         , latitude :: Double
                         , longitude :: Double
                         , elevation :: Double
                         } deriving (Show, Eq)

type Metadata = [(String, String)]

data DeviceList = DeviceList { total :: Int
                             , registered :: Int
                             , unregistered :: Int
                             } deriving (Show, Eq)

data Distribution = Distribution { id :: HexString
                                 , name :: String
                                 , description :: String
                                 , visibility :: Visibility
                                 , status :: Status
                                 , url :: URL
                                 , key :: HexString
                                 , created :: DateTime
                                 , updated :: DateTime
                                 , devices :: DeviceList
                                 } deriving (Show, Eq)

data Device = Device { id :: HexString
                     , url :: URL
                     , name :: String
                     , description :: String
                     , visibility :: Visibility
                     , status :: Status
                     , collections :: [HexString]
                     , serial :: String
                     , tags :: [String]
                     , location :: Waypoint
                     , created :: DateTime
                     , updated :: DateTime
                     } deriving (Show, Eq)

data Collection = Collection { id :: HexString
                             , parent :: Maybe HexString
                             , name :: String
                             , description :: String
                             , devices :: Int
                             , collections :: Int
                             , tags :: [String]
                             , metadata :: Maybe Metadata
                             , key :: HexString
                             , created :: DateTime
                             , updated :: DateTime
                             } deriving (Show, Eq)

data Permission = GET | POST | PUT | DELETE deriving (Show, Eq)
data Key = Key { name :: String
               , key :: HexString
               , master :: Bool
               , expiresAt :: Maybe DateTime
               , expired :: Bool
               , origin :: [IP]
               , permissions :: [Permission]
               } deriving (Show, Eq)

data State = Queued | Working | Complete | Failed deriving (Show, Eq)

data Job = Job { id:: String
               , state :: State
               , output :: String
               , errors :: [String]
               , started :: Maybe DateTime
               , finished :: Maybe DateTime
               , created :: DateTime
               , updated :: DateTime
               } deriving (Show, Eq)
