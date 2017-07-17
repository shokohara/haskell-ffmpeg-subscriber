{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module AppSpec where

import Api
import App
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception (throwIO)
import Control.Exception hiding (try)
import Control.Monad
import Data.Aeson
import Data.IORef
import Data.String.Here
import Debug.Trace
import Network.HTTP.Client (ManagerSettings(..))
import Network.HTTP.Client (Manager, responseTimeoutMicro, newManager, defaultManagerSettings)
import Network.Wai (Application)
import Network.Wai.Handler.Warp
import Option hiding (dir)
import Servant
import Servant.Client
import State
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

m :: IORef State
m = unsafePerformIO $ newIORef (State [])

--dir = [i|${cwd}/tmp|]

waitAll :: IORef State -> IO ()
waitAll i = void $ readIORef i >>= (\x-> sequence (wait <$> values x))

spec :: Spec
spec =
  describe "App" $ do
    it "decode . encode" $ do
      (decode . encode) (PubSubRequest (Message (Attributes "1" (StorageKey "#") "google.com") "" "" "") "") `shouldBe` (Nothing :: Maybe PubSubRequest)
      (decode . encode) (PubSubRequest (Message (Attributes "1" (StorageKey "[") "google.com") "" "" "") "") `shouldBe` (Nothing :: Maybe PubSubRequest)
      (decode . encode) (PubSubRequest (Message (Attributes "1" (StorageKey "]") "google.com") "" "" "") "") `shouldBe` (Nothing :: Maybe PubSubRequest)
      (decode . encode) (PubSubRequest (Message (Attributes "1" (StorageKey "*") "google.com") "" "" "") "") `shouldBe` (Nothing :: Maybe PubSubRequest)
      (decode . encode) (PubSubRequest (Message (Attributes "1" (StorageKey "?") "google.com") "" "" "") "") `shouldBe` (Nothing :: Maybe PubSubRequest)
--    withClient (mkApp (Option 3000 dir "shokoharatest" 1) m) $ do
--      it "status 0" $ \host -> do
--        try host getStatus `shouldReturn` [Val 0]
--      it "payload" $ \host -> do
--        try host (postPayload $ PubSubRequest (Message (Attributes "1" (StorageKey "shokohara") "https://google.com/playlist.m3u8") "" "" "") "") `shouldThrow` anyException
--      it "allows to show items by id" $ \host -> do
--        removeDirectoryRecursive [i|${cwd}/tmp|] `catch` \e -> putStrLn ("Caught " ++ show (e :: SomeException))
--        waitAll m `catch` \e -> putStrLn ("Caught " ++ show (e :: IOException))
--        try host (postPayload $ PubSubRequest (Message (Attributes "1" (StorageKey "shokohara") "https://google.com/playlist.m3u8") "" "" "") "")
--        try host getStatus `shouldReturn` [Val 1]
--        waitAll m `catch` \e -> putStrLn ("Caught " ++ show (e :: IOException))
--        try host getStatus `shouldReturn` [Val 0]
--
--withClient :: IO Application -> SpecWith Host -> SpecWith ()
--withClient x innerSpec =
--  beforeAll (newManager $ defaultManagerSettings { managerResponseTimeout = responseTimeoutMicro 10000 } ) $
--    flip aroundWith innerSpec $ \action manager ->
--      testWithApplication x $ \p ->
--        action (manager, BaseUrl Http "localhost" p "")
--
--try :: Host -> ClientM a -> IO a
--try (manager, baseUrl) c = either throwIO return =<< runClientM c (ClientEnv manager baseUrl)

