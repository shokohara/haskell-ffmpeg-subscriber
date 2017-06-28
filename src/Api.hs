{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Api where

import Data.Aeson
import Data.Aeson.Casing
import Data.Maybe
import Data.Text (Text)
import GHC.Generics
import Prelude hiding (lookup)
import Servant

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

data Val = Val { name :: String, value :: Int } deriving (Show, Generic, FromJSON, ToJSON, Eq)

type ClientApi = "api" :> Get '[JSON] (Maybe Val)

type Api = "status" :> Get '[JSON] [Val]
  :<|> "payload" :> ReqBody '[JSON] PubSubRequest :> Post '[JSON] ()

clientApi :: Proxy ClientApi
clientApi = Proxy

api :: Proxy Api
api = Proxy

