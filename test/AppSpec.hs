{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module AppSpec where

import Control.Exception (throwIO)
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Network.HTTP.Types
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

cwd :: FilePath
cwd = unsafePerformIO getCurrentDirectory

spec :: Spec
spec = do
  describe "/item" $ do
    withClient (mkApp (Option 2999 [i|${cwd}/tmp|] "shokoharatest") (unsafePerformIO $ newIORef (State []))) $ do
      it "allows to show items by id" $ \host -> do
        try host getStatus `shouldReturn` [Val "localhost" 0]
    withClient (mkApp (Option 3000 [i|${cwd}/tmp|] "shokoharatest") (unsafePerformIO $ newIORef (State []))) $ do
      it "allows to show items by id" $ \host -> do
        --print cwd
        try host (postPayload $ PubSubRequest (Message (Attributes "1" "thisiskey" "google.com") "" "" "") "") `shouldReturn` ()
----      it "allows to show items by id" $ \ host -> do
----        try host (getItem 0) `shouldReturn` Item 0 "example item"
--        try host (getItem) `shouldReturn` Item 0 "example item"
----      it "throws a 404 for missing items" $ \ host -> do
----        try host (getItem 42) `shouldThrow` (\ e -> responseStatus e == notFound404)

withClient :: IO Application -> SpecWith Host -> SpecWith ()
withClient x innerSpec =
  beforeAll (newManager defaultManagerSettings) $ do
    flip aroundWith innerSpec $ \action -> \manager -> do
      testWithApplication x $ \port -> do
        action (manager, BaseUrl Http "localhost" port "")

try :: Host -> ClientM a -> IO a
try (manager, baseUrl) client = either throwIO return =<< runClientM client (ClientEnv manager baseUrl)

