{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module App where

import Api
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

getAllIps :: ClientM (Maybe Val)
getAllIps = client clientApi

traceIp :: String -> IO (Either ServantErr (Maybe Val))
traceIp a = do
  manager <- liftIO $ newManager defaultManagerSettings
  mapBoth (const err500) id <$> runClientM getAllIps (ClientEnv manager (BaseUrl Http a 80 ""))

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

-- ffmpeg -i movieurl -acodec copy -vcodec copy -f segment -segment_time 5 -segment_list playlist.m3u8 %d.ts
ffmpegCommand :: Option -> PubSubRequest -> (String, [String])
ffmpegCommand o r =
  let dirPath = O.dir o
      key = attributesKey . messageAttributes . psrMessage $ r
      url = attributesUrl . messageAttributes . psrMessage $ r
   --in ("ffmpeg", [url, key])
  in ("touch", [[i|${dirPath}/${key}/${key}.m3u8|]])

server :: Option -> MVar State -> Server Api
server o s = statusHandler s :<|> payloadHandler o s where
  statusHandler :: MVar State -> Handler [Val]
  statusHandler s = lift $ status s
  payloadHandler :: Option -> MVar State -> PubSubRequest -> Handler ()
  payloadHandler o s a = lift $ payload o s a
  status :: MVar State -> IO [Val]
  status s = do
    m <- readMVar s
    (\n -> [Val "localhost" n]) . length . filter isNothing <$> sequence (getExitCode <$> values m)
  -- 処理不可能な引数で落ちる
  payload :: Option -> MVar State -> PubSubRequest -> IO ()
  payload o s a = do
    m <- takeMVar s
    print . length $ values m
    sequence (getExitCode <$> values m) >>= print
    x <- createProcess (proc "sh" ["-c", "sleep 3; date >> abc; echo finish"])
    (_,_,_,z) <- createProcess (uncurry proc (mkdirCommand o a))
    _ <- waitForProcess z
    y <- createProcess (uncurry proc (ffmpegCommand o a))
    putMVar s $ State $ values m ++ [x, y]
    return ()

mkApp :: Option -> MVar State -> IO Application
mkApp o s = return $ serve api (server o s)

run :: MVar State -> Option -> IO ()
run s o = withStdoutLogger $ \apilogger -> do
  let settings =
        setPort (O.port o) $
          setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show (O.port o))) $
            setLogger apilogger defaultSettings
  runSettings settings =<< mkApp o s

