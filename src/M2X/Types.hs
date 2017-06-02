{-# LANGUAGE DuplicateRecordFields #-}

module M2X.Types where

import Data.HexString
import Data.DateTime
import Data.IP

data Permission = GET | POST | PUT | DELETE deriving (Show, Eq)
data Key = Key { name :: String
               , key :: HexString
               , master :: Bool
               , expiresAt :: Maybe DateTime
               , expired :: Bool
               , origin :: [IP]
               , permissions :: [Permission]} deriving (Show, Eq)

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
