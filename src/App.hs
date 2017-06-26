{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module App where

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

-- Mapになるだろう
-- ffmpegが完了したら完了とみなされてしまう（GCPにアップロードする必要があるのに
newtype State = State {
  values :: [(Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)]
                   }
newtype StateVar = StateVar (MVar State)

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

type ServerApi = "status" :> Get '[JSON] [Val]
  :<|> "payload" :> ReqBody '[JSON] PubSubRequest :> Post '[JSON] ()

clientApi :: Proxy ClientApi
clientApi = Proxy

serverApi :: Proxy ServerApi
serverApi = Proxy

getAllBooks :: ClientM (Maybe Val)
getAllBooks = client clientApi

apiApi :: String -> IO (Either ServantErr (Maybe Val))
apiApi a = do
  manager <- liftIO $ newManager defaultManagerSettings
  mapBoth (const err500) id <$> runClientM getAllBooks (ClientEnv manager (BaseUrl Http a 80 ""))

statusApi :: MVar State -> IO [Val]
statusApi s = do
  m <- readMVar s
  (\n -> [Val "localhost" n]) . length . filter isNothing <$> sequence (getExitCode <$> values m)

apiHandler :: MVar State -> Handler [Val]
apiHandler s = lift $ statusApi s

try' :: IO a -> IO (Either IOException a)
try' = try

get4 :: (t2, t1, t, t3) -> t3
get4 (_, _, _, x) = x

get2 :: (t2, t3, t1, t) -> t3
get2 (_, x, _, _) = x

getStdOut :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO String
getStdOut = hGetContents . fromJust . get2

getExitCode :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO (Maybe ExitCode)
getExitCode = getProcessExitCode . get4

mkdirCommand :: Option -> PubSubRequest -> (String, [String])
mkdirCommand o r =
  let dirPath = O.dir o
      key = attributesKey . messageAttributes . psrMessage $ r
      url = attributesUrl . messageAttributes . psrMessage $ r
  in ("mkdir", ["-p", [i|${dirPath}/${key}|]])

ffmpegCommand :: Option -> PubSubRequest -> (String, [String])
ffmpegCommand o r =
  let dirPath = O.dir o
      key = attributesKey . messageAttributes . psrMessage $ r
      url = attributesUrl . messageAttributes . psrMessage $ r
   --in ("ffmpeg", [url, key])
  in ("touch", [[i|${dirPath}/${key}/${key}.m3u8|]])

-- 処理不可能な引数で落ちる
payloadApi :: Option -> MVar State -> PubSubRequest -> IO ()
payloadApi o s a = do
  m <- takeMVar s
  print . length $ values m
  sequence (getExitCode <$> values m) >>= print
  x <- createProcess (proc "sh" ["-c", "sleep 3; date >> abc; echo finish"])
  (_,_,_,z) <- createProcess (uncurry proc (mkdirCommand o a))
  _ <- waitForProcess z
  y <- createProcess (uncurry proc (ffmpegCommand o a))
  putMVar s $ State $ values m ++ [x, y]
  return ()

payloadHandler :: Option -> MVar State -> PubSubRequest -> Handler ()
payloadHandler o s a = lift $ payloadApi o s a

server :: Option -> MVar State -> Server ServerApi
server o s = apiHandler s :<|> payloadHandler o s

mkApp :: Option -> MVar State -> IO Application
mkApp o s = return $ serve serverApi (server o s)

run :: MVar State -> Option -> IO ()
run s o = withStdoutLogger $ \apilogger -> do
  let settings =
        setPort (O.port o) $
          setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show (O.port o))) $
            setLogger apilogger defaultSettings
  runSettings settings =<< mkApp o s

