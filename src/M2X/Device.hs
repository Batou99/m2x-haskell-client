{-# LANGUAGE OverloadedStrings #-}

module M2X.Device where

import M2X.Types
import Network.Wreq
import Data.Aeson
import Control.Exception as E
import Control.Lens
import Network.URL
import Data.HexString

getCatalog :: HexString -> URL -> [Device]
getCatalog masterKey url =
  undefined

getDevice :: HexString -> URL -> Device
getDevice masterKey url =
  undefined
