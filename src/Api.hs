{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Api where

import System.Exit
import Control.Exception hiding (Handler)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class    (lift)
import Data.Aeson
import Data.Aeson.Casing
import Data.Either.Combinators
import Data.String.Here
import Data.Maybe
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Client hiding (Proxy)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger       (withStdoutLogger)
import Option (Option)
import Prelude hiding (lookup)
import Data.Map (fromList)
import Servant
import Servant.Client
import System.IO
import System.Process
import qualified Option as O
import Control.Concurrent.MVar
import Data.String

data Attributes = Attributes { attributesVersion :: String, attributesKey :: String, attributesUrl :: String } deriving (Show, Eq, Generic)
data Message = Message { messageAttributes :: Attributes, messageData :: Text, messageMessageId :: Text, messagePublishTime :: Text } deriving (Show, Eq, Generic)
data PubSubRequest = PubSubRequest { psrMessage :: Message, psrSubscription :: Text } deriving (Show, Eq, Generic)

instance ToJSON Attributes where
  toJSON = genericToJSON $ aesonPrefix snakeCase
instance ToJSON Message where
  toJSON = genericToJSON $ aesonPrefix snakeCase
instance ToJSON PubSubRequest where
  toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON Attributes where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
instance FromJSON Message where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
instance FromJSON PubSubRequest where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Val = Val { name :: String, value :: Int } deriving (Show, Generic, FromJSON, ToJSON)

type ClientApi = "api" :> Get '[JSON] (Maybe Val)

type Api = "status" :> Get '[JSON] [Val]
  :<|> "payload" :> ReqBody '[JSON] PubSubRequest :> Post '[JSON] ()

clientApi :: Proxy ClientApi
clientApi = Proxy

api :: Proxy Api
api = Proxy

