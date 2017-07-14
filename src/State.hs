{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module State where

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
import           System.Directory
import           System.Directory.Extra
--import           System.Exit
import           System.FilePath.Posix
import           System.IO
import           System.IO.Unsafe
import           System.Process hiding (cwd)

newtype State = State { values :: [IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle, IO ())] }
