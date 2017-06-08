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
  let filter = Location $ LocationNone NoLocation
  res <- run $ catalogSearch key (Just "MoviePoster") Nothing Nothing Nothing Nothing Nothing Nothing Nothing (FilterReqBody [filter])
  case res of 
    Left err -> putStrLn $ "Error: " ++ show err
    Right t -> print t
