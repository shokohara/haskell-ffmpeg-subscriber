{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module App where

import qualified Option as O
import Option (Option)
import     Control.Monad
import Data.Either.Combinators
import           Control.Monad.Trans.Class    (lift)
import Control.Monad.IO.Class (liftIO)
import           Data.Aeson
import Data.String.Here
import           GHC.Generics
import           Network.Wai
import Servant.Client
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO
import System.Process
import Network.HTTP.Client hiding (Proxy)
import           Network.Wai.Logger       (withStdoutLogger)
import Control.Exception hiding (Handler)
import GHC.Generics
import Data.Text (Text)
import Data.Aeson
import Data.Aeson.Casing
import Control.Concurrent.Async

data Message = Message { messageAttributes :: Object, messageData :: Text, messageMessageId :: Text, messagePublishTime :: Text } deriving (Show, Eq, Generic)
data PubSubRequest = PubSubRequest { psrMessage :: Message, psrSubscription :: Text } deriving (Show, Eq, Generic)

instance ToJSON Message where
  toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON Message where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
instance ToJSON PubSubRequest where
  toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON PubSubRequest where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Val = Val { name :: String, value :: Int } deriving (Show, Generic, FromJSON, ToJSON)

type ClientApi = "api" :> Get '[JSON] (Maybe Val)

type ServerApi = "api" :> Capture "ip" String :> Get '[JSON] (Maybe Val)
  :<|> "root" :> ReqBody '[JSON] PubSubRequest :> Post '[JSON] ()

clientApi :: Proxy ClientApi
clientApi = Proxy

serverApi :: Proxy ServerApi
serverApi = Proxy

getAllBooks :: ClientM (Maybe Val)
getAllBooks = client clientApi

appMain3 :: String -> IO (Either ServantErr (Maybe Val))
appMain3 a = do
  manager <- liftIO $ newManager defaultManagerSettings
  mapBoth (const err500) id <$> runClientM getAllBooks (ClientEnv manager (BaseUrl Http a 80 ""))

try' :: IO a ->  IO (Either IOException a)
try' =  try

get2nd (_,b,_,_)=b

test :: String -> IO ()
test a = do
  a1 <- async (get2nd <$> createProcess (proc "sh" ["-c", [here|"sleep 100; touch $a"|]]))
  r1 <- wait a1
  print r1

appMain4 :: PubSubRequest -> IO ()
appMain4 a = do
--  result <- try' $ createProcess (proc "sh" ["-c", "sleep 1; touch abc"])
  (_, Just parted, _, _) <- createProcess (proc "sh" ["-c", "sleep 100; touch abc"])
  a <- hGetContents parted
  x <- print a
  return x

app3 :: String -> Handler (Maybe Val)
app3 a = lift $ join . rightToMaybe <$> appMain3 a

app4 :: PubSubRequest -> Handler ()
app4 a = lift $ appMain4 a

server :: Server ServerApi
server = app3 :<|> app4

mkApp :: IO Application
mkApp = return $ serve serverApi server

run :: TVar [IO ()] -> Option -> IO ()
run s o = withStdoutLogger $ \apilogger -> do
  let settings =
        setPort (O.port o) $
          setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show (O.port o))) $
            setLogger apilogger defaultSettings
  runSettings settings =<< mkApp

