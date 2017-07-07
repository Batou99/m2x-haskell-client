module Main where

import M2X.Time (time)
import M2X.DeviceAPI
import M2X.DeviceTypes
import M2X.Client (run)
import System.Environment (lookupEnv)

main :: IO ()
main = do
  key <- lookupEnv "KEY"
  res <- run time
  case res of 
    Left err -> putStrLn $ "Error: " ++ show err
    Right t -> print t
  {-res <- run $ getCatalog key-}
  {-case res of -}
    {-Left err -> putStrLn $ "Error: " ++ show err-}
    {-Right t -> print t-}
  let point = LocationPoint 0 0
  let circle = LocationWithinCircle (LocationPoint 0 0) Km 0.1
  let params = defaultCatalogSearchParams { cspReqBody = FilterReqBody [Location circle] }
  res <- run $ catalogSearchFromParams params
  case res of 
    Left err -> putStrLn $ "Error: " ++ show err
    Right t -> print t
