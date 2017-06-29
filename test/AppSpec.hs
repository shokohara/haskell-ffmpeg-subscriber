{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module AppSpec where

import Control.Exception (throwIO)
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Network.Wai (Application)
import Network.Wai.Handler.Warp
import Data.String.Here
import Servant
import Servant.Client
import Test.Hspec
import System.Directory
import App
import Api
import Option
import System.IO.Unsafe
import Data.IORef

type Host = (Manager, BaseUrl)

getStatus :: ClientM [Val]
postPayload :: PubSubRequest -> ClientM ()
getStatus :<|> postPayload = client api

{-# NOINLINE cwd #-}
cwd :: FilePath
cwd = unsafePerformIO getCurrentDirectory

spec :: Spec
spec =
  describe "/item" $ do
    withClient (mkApp (Option 2999 [i|${cwd}/tmp|] "shokoharatest") (unsafePerformIO $ newIORef (State []))) $
      it "allows to show items by id" $ \host ->
        try host getStatus `shouldReturn` [Val "localhost" 0]
    withClient (mkApp (Option 3000 [i|${cwd}/tmp|] "shokoharatest") (unsafePerformIO $ newIORef (State []))) $
      it "allows to show items by id" $ \host ->
        try host (postPayload $ PubSubRequest (Message (Attributes "1" "thisiskey" "google.com") "" "" "") "") `shouldReturn` ()
    withClient (mkApp (Option 3000 [i|${cwd}/tmp|] "shokoharatest") (unsafePerformIO $ newIORef (State []))) $
      it "allows to show items by id" $ \host ->
        try host (postPayload $ PubSubRequest (Message (Attributes "1" "#[]*?" "google.com") "" "" "") "") `shouldReturn` ()

withClient :: IO Application -> SpecWith Host -> SpecWith ()
withClient x innerSpec =
  beforeAll (newManager defaultManagerSettings) $
    flip aroundWith innerSpec $ \action manager ->
      testWithApplication x $ \p ->
        action (manager, BaseUrl Http "localhost" p "")

try :: Host -> ClientM a -> IO a
try (manager, baseUrl) c = either throwIO return =<< runClientM c (ClientEnv manager baseUrl)

