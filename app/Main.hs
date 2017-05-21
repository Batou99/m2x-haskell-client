module Main where

import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import M2X.Time (time, Time)
import Servant.Client

query :: ClientM Time
query = time

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  res <- runClientM query (ClientEnv manager (BaseUrl Https "api-m2x.att.com" 443 ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right t -> print t
