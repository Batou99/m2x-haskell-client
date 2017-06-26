module Main where

import M2X.Time (time)
import M2X.Device 
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
  let params = defaultCatalogSearchParams { cspName = Just "MoviePoster"
                                          , cspReqBody = FilterReqBody [Location NoLocation] }
  res <- run $ catalogSearchFromParams params
  case res of 
    Left err -> putStrLn $ "Error: " ++ show err
    Right t -> print t
