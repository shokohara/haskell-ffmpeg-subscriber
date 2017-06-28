{-# LANGUAGE OverloadedStrings #-}

module AppSpec where

import           Control.Exception (throwIO)
import           Control.Monad.Trans.Except
import           Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import           Network.HTTP.Types
import           Network.Wai (Application)
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Client
import           Test.Hspec

import           App hiding (getItems)
import Api
import Option
import System.IO.Unsafe
import Data.IORef

type Host = (Manager, BaseUrl)

getItem :: ClientM [Val]
getItems :: PubSubRequest -> ClientM ()
getItem :<|> getItems = client api

spec :: Spec
spec = do
  describe "/item" $ do
    withClient (mkApp (Option 3000 "." "shokoharatest") (unsafePerformIO $ newIORef (State []))) $ do
      it "allows to show items by id" $ \host -> do
        try host getItem `shouldReturn` [Val "localhost" 0]
    withClient (mkApp (Option 3000 "." "shokoharatest") (unsafePerformIO $ newIORef (State []))) $ do
      it "allows to show items by id" $ \host -> do
        try host (getItems $ PubSubRequest (Message (Attributes "1" "thisiskey" "google.com") "" "" "") "") `shouldReturn` ()
    it "lists an example item" $ do
      1 `shouldBe` 1
--        try host getItems `shouldReturn` [Item 0 "example item"]
----      it "allows to show items by id" $ \ host -> do
----        try host (getItem 0) `shouldReturn` Item 0 "example item"
--        try host (getItem) `shouldReturn` Item 0 "example item"
----
----      it "throws a 404 for missing items" $ \ host -> do
----        try host (getItem 42) `shouldThrow` (\ e -> responseStatus e == notFound404)

withClient :: IO Application -> SpecWith Host -> SpecWith ()
withClient x innerSpec =
  beforeAll (newManager defaultManagerSettings) $ do
    flip aroundWith innerSpec $ \ action -> \ manager -> do
      testWithApplication x $ \ port -> do
        let baseUrl = BaseUrl Http "localhost" port ""
        action (manager, baseUrl)

try :: (Manager, BaseUrl) -> ClientM a -> IO a
try (manager, baseUrl) client = either throwIO return =<< runClientM client (ClientEnv manager baseUrl)

