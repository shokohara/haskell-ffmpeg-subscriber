{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module App where

import           Api
import Api.Payload
import           Api.Status
import Control.Concurrent.Async
import           Control.Exception hiding (Handler)
import           Control.Lens ((&), (.~), (<&>), (?~))
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Either.Combinators
import           Data.IORef
import           Data.Maybe
import           Data.String.Here
import qualified Data.Text as T
import           Data.Text (Text, unpack)
import           Debug.Trace
import qualified Network.Google as Google
import qualified Network.Google.Storage as Storage
import           Network.HTTP.Client hiding (Proxy)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger       (withStdoutLogger)
import           Option
import           Option (Option)
import qualified Option as O
import           Prelude hiding (lookup)
import           Servant
import           Servant.Client
import State
import           System.Directory
import           System.Directory.Extra
import           System.Exit
import           System.FilePath.Posix
import           System.IO
import           System.IO.Unsafe
import           System.Process hiding (cwd)

--newtype State = State { values :: [IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle, IO ())] }
--newtype State = State { values :: [(IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle), IO ())] }

getAllIps :: ClientM (Maybe Val)
getAllIps = client clientApi

traceIp :: String -> IO (Either ServantErr (Maybe Val))
traceIp a = do
  manager <- liftIO $ newManager defaultManagerSettings
  mapBoth (const err500) id <$> runClientM getAllIps (ClientEnv manager (BaseUrl Http a 80 ""))

try' :: IO a -> IO (Either IOException a)
try' = try

server :: Option -> IORef State -> Server Api
server o s = statusHandler s :<|> payloadHandler o s where

--testFunction :: IO ((), ())
--testFunction :: IORef [(Async (), Async ())] -> IO ()
--testFunction = do
--  pa <- async (putStrLn "start to download" >>= \_ -> putStrLn "downloading" >>= \_ -> threadDelay 1000 >>= \_ -> putStrLn "finish downloading")
--  pb <- async (putStrLn "start to upload" >>= \_ -> threadDelay 1000 >>= \_ -> putStrLn "uploading" >>= \_ -> putStrLn "finish uploading")
--  return (pa, pb)
--testFunction :: IO (Async (), Async ())

mkApp :: Option -> IORef State -> IO Application
mkApp o s = return $ serve api (server o s)

run :: IORef State -> Option -> IO ()
run s o = withStdoutLogger $ \apilogger -> do
  let settings =
        setPort (O.port o) $
          setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show (O.port o))) $
            setLogger apilogger defaultSettings
  runSettings settings =<< mkApp o s

