{-# LANGUAGE UndecidableInstances #-}
module M2X.Client where

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client
import Data.HexString
import Servant.Common.Req (Req, addHeader)


apiUrl :: String
apiUrl = "api-m2x.att.com"

run :: ClientM a -> IO (Either ServantError a)
run query = do
  manager <- newManager tlsManagerSettings
  runClientM query (ClientEnv manager (BaseUrl Https apiUrl 443 ""))
