{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module App where

import Control.Concurrent.Async
import Control.Concurrent.STM.TVar
import Control.Exception hiding (Handler)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Control.Monad.STM
import Control.Monad.Trans.Class    (lift)
import Data.Aeson
import Data.Aeson.Casing
import Data.Either.Combinators
import Data.String.Here
import Data.Text (Text)
import GHC.Generics
import GHC.Generics
import Network.HTTP.Client hiding (Proxy)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger       (withStdoutLogger)
import Option (Option)
import Prelude hiding (lookup)
import STMContainers.Map
import Data.Map (fromList)
import Servant
import Servant.Client
import System.IO
import System.Process
import qualified Option as O
import Control.Concurrent.MVar

data State = State {
  values :: [Int]
                   } deriving Show
newtype StateVar = StateVar (MVar State)

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

--try' :: IO a ->  IO (Either IOException a)
--try' =  try

--get2nd (_,b,_,_)=b

--test :: String -> IO ()
--test a = do
--  a1 <- async (get2nd <$> createProcess (proc "sh" ["-c", [here|"sleep 100; touch $a"|]]))
--  r1 <- wait a1
--  print r1
--
--askTest :: Map Integer v -> STM (Maybe v)
--askTest = STMContainers.Map.lookup $ toInteger 1

--ask4 = do
--  a <- ask

calculateContentLen :: Reader String Int
calculateContentLen = do
  content <- ask
  return $ length content
--ask3 = do
--  a <- ask
--  askTest a
--ask2 :: Map Integer Integer
--ask2 = fromList $ [(toInteger 1, toInteger 1)]

--handler :: TVar (Map k a) -> PubSubRequest -> STM ()
--handler s r = do
--  currentValue <- readTVar s
--  writeTVar s $ (Map 1 1)

data Configg = Configg { myState :: TVar (Int, Int) }

--test2 :: String -> Map Integer (IO ())
--test2 a = do
--  state <- ask
--  state
-- :: TVar (Int, Int)
--abc = do
--  Configg { myState = state } <- ask
--  atomically $ do
--    currentValue <- readTVar state
--    writeTVar state currentValue
--  return ()

appMain4 :: (MVar State) -> PubSubRequest -> IO ()
appMain4 s a = do
  m <- takeMVar s
  print m
--  result <- try' $ createProcess (proc "sh" ["-c", "sleep 1; touch abc"])
 -- (_, Just parted, _, _) <- createProcess (proc "sh" ["-c", "sleep 100; touch abc"])
 -- a <- hGetContents parted
 -- x <- print a
--  return x
  return ()

app3 :: String -> Handler (Maybe Val)
app3 a = lift $ join . rightToMaybe <$> appMain3 a

app4 :: (MVar State) -> PubSubRequest -> Handler ()
app4 s a = lift $ appMain4 s a

server :: (MVar State) -> Server ServerApi
server s = app3 :<|> app4 s

mkApp :: (MVar State) -> IO Application
mkApp s = return $ serve serverApi (server s)

run :: (MVar State) -> Option -> IO ()
run s o = withStdoutLogger $ \apilogger -> do
  let settings =
        setPort (O.port o) $
          setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show (O.port o))) $
            setLogger apilogger defaultSettings
  runSettings settings =<< mkApp s

