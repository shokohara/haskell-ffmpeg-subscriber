{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Api.PayloadSpec where

import Api
import Api.Payload hiding (cwd)
import App
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Exception (throwIO)
import Control.Monad
import Data.IORef
import Data.Maybe
import Data.String.Here
import Data.Text
import Data.UUID
import Debug.Trace
import Network.Wai (Application)
import Network.Wai.Handler.Warp
import Option
import System.Directory
import System.IO.Unsafe
import Test.Hspec

{-# NOINLINE cwd #-}
cwd :: FilePath
cwd = unsafePerformIO getCurrentDirectory

option = Option 3000 [i|${cwd}/tmp|] "shokoharatest" 1

spec :: Spec
spec =
  describe "Api.Payload" $ do
    it "createTmpFile" $ do
      let uuid = unsafePerformIO newUUID in do
          (void $ createTmpFile "shokoharatest" (PubSubRequest (Message (Attributes "1" (StorageKey "testkey") "google.com") "" "" "") "") uuid) `shouldReturn` ()
    it "tmpFileName" $ do
      tmpFileName (PubSubRequest (Message (Attributes "1" (StorageKey "testkey") "google.com") "" "" "") "") (fromJust . fromString $ "00000000-0000-0000-0000-000000000000") `shouldBe` pack ("uuid-00000000-0000-0000-0000-000000000000-testkey/tmpfile")
    it "deleteTmpFile" $ do
      let uuid = unsafePerformIO newUUID in do
          (void $ createTmpFile "shokoharatest" (PubSubRequest (Message (Attributes "1" (StorageKey "testkey") "google.com") "" "" "") "") uuid) `shouldReturn` ()
          deleteTmpFile "shokoharatest" (PubSubRequest (Message (Attributes "1" (StorageKey "testkey") "google.com") "" "" "") "") uuid `shouldReturn` ()
    it "function2" $ do
      let uuid = unsafePerformIO newUUID in
          name (PubSubRequest (Message (Attributes "1" (StorageKey "testkey") "google.com") "" "" "") "") uuid "a" `shouldBe` (pack $ "uuid-" ++ show uuid ++ "-" ++ "testkey" ++ "/" ++ "a")
    it "mkdir" $ do
      removeDirectoryRecursive [i|${cwd}/tmp|] `catch` \e -> putStrLn ("Caught " ++ show (e :: SomeException))
      mkdir option (PubSubRequest (Message (Attributes "1" (StorageKey "testkey") "google.com") "" "" "") "")
      listDirectory [i|${cwd}/tmp|] `shouldReturn` ["testkey"]
    it "rm" $ do
      removeDirectoryRecursive [i|${cwd}/tmp|] `catch` \e -> putStrLn ("Caught " ++ show (e :: SomeException))
      mkdir option (PubSubRequest (Message (Attributes "1" (StorageKey "testkey") "google.com") "" "" "") "")
      rm option (PubSubRequest (Message (Attributes "1" (StorageKey "testkey") "google.com") "" "" "") "")
      listDirectory [i|${cwd}/tmp|] `shouldReturn` []

