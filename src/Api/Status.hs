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
statusHandler = liftIO . status where
  status :: IORef State -> IO [Val]
  status s = do
    m <- readIORef s :: IO State
    b <- f3 (values m) :: IO [Bool]
    return [Val (length . filter (== True) $ b)]
  f :: IO (Async a) -> IO Bool
  f i = isNothing <$> (i >>= poll)
  f1 :: Async a -> IO Bool
  f1 i = isNothing <$> poll i
  f2 :: [Async a] -> [IO Bool]
  f2 = (\x -> f1 <$> x)
  f3 :: [Async a] -> IO [Bool]
  f3 x = sequence $ f2 x
  f4 :: [Async a] -> IO [Maybe (Either SomeException a)]
  f4 x = sequence $ f5 x
  f5 :: [Async a] -> [IO (Maybe (Either SomeException a))]
  f5 = (\x -> poll <$> x)

