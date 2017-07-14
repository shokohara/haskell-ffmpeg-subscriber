{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module AppSpec where

import Api
import App
import Control.Exception (throwIO)
import Data.IORef
import Data.String.Here
import Debug.Trace
import Network.HTTP.Client (ManagerSettings(..))
import Network.HTTP.Client (Manager, responseTimeoutMicro, newManager, defaultManagerSettings)
import Network.Wai (Application)
import Network.Wai.Handler.Warp
import Option
import Servant
import Servant.Client
import System.Directory
import System.IO.Unsafe
import Test.Hspec

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
    withClient (mkApp (Option 3000 [i|${cwd}/tmp|] "shokoharatest" 1) (unsafePerformIO $ newIORef (State []))) $
      it "allows to show items by id" $ \host ->
        try host getStatus `shouldReturn` [Val "localhost" 0]
    withClient (mkApp (Option 3000 [i|${cwd}/tmp|] "shokoharatest" 1) (unsafePerformIO $ newIORef (State []))) $
      it "allows to show items by id" $ \host ->
        try host (postPayload $ PubSubRequest (Message (Attributes "1" (StorageKey "thisiskey") "google.com") "" "" "") "") `shouldReturn` ()
    withClient (mkApp (Option 3000 [i|${cwd}/tmp|] "shokoharatest" 1) (unsafePerformIO $ newIORef (State []))) $ do
      it "allows to show items by id" $ \host ->
        try host (postPayload $ PubSubRequest (Message (Attributes "1" (StorageKey "#") "google.com") "" "" "") "") `shouldThrow` anyException
      it "allows to show items by id" $ \host ->
        try host (postPayload $ PubSubRequest (Message (Attributes "1" (StorageKey "[") "google.com") "" "" "") "") `shouldThrow` anyException
      it "allows to show items by id" $ \host ->
        try host (postPayload $ PubSubRequest (Message (Attributes "1" (StorageKey "]") "google.com") "" "" "") "") `shouldThrow` anyException
      it "allows to show items by id" $ \host ->
        try host (postPayload $ PubSubRequest (Message (Attributes "1" (StorageKey "*") "google.com") "" "" "") "") `shouldThrow` anyException
      it "allows to show items by id" $ \host ->
        try host (postPayload $ PubSubRequest (Message (Attributes "1" (StorageKey "?") "google.com") "" "" "") "") `shouldThrow` anyException
    withClient (mkApp (Option 3000 [i|${cwd}/tmp|] "shokoharatest" 1) (unsafePerformIO $ newIORef (State []))) $
      it "allows to show items by id" $ \host -> do
        try host getStatus `shouldReturn` [Val "localhost" 0]

withClient :: IO Application -> SpecWith Host -> SpecWith ()
withClient x innerSpec =
  beforeAll (newManager $ defaultManagerSettings { managerResponseTimeout = responseTimeoutMicro 10000 } ) $
    flip aroundWith innerSpec $ \action manager ->
      testWithApplication x $ \p ->
        action (manager, BaseUrl Http "localhost" p "")

try :: Host -> ClientM a -> IO a
try (manager, baseUrl) c = either throwIO return =<< runClientM c (ClientEnv manager baseUrl)

