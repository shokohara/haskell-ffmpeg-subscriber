{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Api.Status where

import           Api
import Control.Concurrent
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
import           System.FilePath.Posix
import           System.IO
import           System.IO.Unsafe
import           System.Process hiding (cwd)

statusHandler :: IORef State -> Handler [Val]
statusHandler = liftIO . status

status :: IORef State -> IO [Val]
status s = do
  m <- readIORef s
--    _ <- traceIO $ show ((unsafePerformIO . (>>= getProcessExitCode) . get4) <$> values m)
--_ <- return $ (>>= traceIO . show) <$> ((>>= getProcessExitCode . get4) <$> values m)
--    _ <- traceIO . show . unsafePerformIO . getProcessExitCode . get4 . fst . unsafePerformIO . head . values $ m
--    _ <- traceIO . show . unsafePerformIO . getProcessExitCode . get4 . unsafePerformIO . head . values $ m
  _ <- traceIO "thisistest"
--    _ <- sequence ((>>= getProcessExitCode . get4) <$> values m)
--    _ <- (\n -> [Val "localhost" n]) . length . filter isNothing <$> sequence ((>>= getProcessExitCode . get4) <$> values m)
  return [Val "localhost" 0]

