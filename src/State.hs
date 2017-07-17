{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module State where

import Control.Concurrent.Async
import           Prelude hiding (lookup)
import           System.IO
import           System.IO.Unsafe
import           System.Process hiding (cwd)

newtype State = State { values :: [Async ()] }

