{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module App where

import System.Directory
import Control.Monad.Trans.Resource (runResourceT)
import Api
import qualified Data.Text as T
import System.Exit
import Control.Exception hiding (Handler)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Either.Combinators
import Data.String.Here
import Data.Maybe
import Data.Text (Text)
import Network.HTTP.Client hiding (Proxy)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger       (withStdoutLogger)
import Option (Option)
import Prelude hiding (lookup)
import Servant
import Servant.Client
import System.IO
import System.Process
import qualified Option as O
import Data.IORef
import qualified Network.Google as Google
import qualified Network.Google.Storage as Storage
import Control.Lens ((&), (.~), (<&>), (?~))
import System.Directory.Extra
import System.FilePath.Posix

newtype State = State { values :: [((Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle), IO ExitCode)] }

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

getExitCode :: ((Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle), IO ExitCode) -> IO (Maybe ExitCode)
getExitCode = getProcessExitCode . get4 . fst

-- ffmpeg -i movieurl -acodec copy -vcodec copy -f segment -segment_time 5 -segment_list playlist.m3u8 %d.ts
ffmpegCommand :: Option -> PubSubRequest -> (String, [String])
ffmpegCommand o r = ("touch", [[i|${O.dir o}/${attributesKey . messageAttributes . psrMessage $ r}/${attributesKey . messageAttributes . psrMessage $ r}.m3u8|]])

ffmpegCommand2 :: Option -> PubSubRequest -> (String, [String])
ffmpegCommand2 o r = ("touch", [[i|${O.dir o}/${attributesKey . messageAttributes . psrMessage $ r}/${attributesKey . messageAttributes . psrMessage $ r}.ts|]])

mkdir :: Option -> PubSubRequest -> IO ()
mkdir o r = createDirectoryIfMissing True [i|${O.dir o}/${attributesKey . messageAttributes . psrMessage $ r}|]

listFile :: Option -> PubSubRequest -> IO [FilePath]
listFile o r =
  let dirPath = O.dir o
      key = attributesKey . messageAttributes . psrMessage $ r
      url = attributesUrl . messageAttributes . psrMessage $ r
  in listFiles [i|${dirPath}/${key}|]

server :: Option -> IORef State -> Server Api
server o s = statusHandler s :<|> payloadHandler o s where
  statusHandler :: IORef State -> Handler [Val]
  statusHandler = liftIO . status
  payloadHandler :: Option -> IORef State -> PubSubRequest -> Handler ()
  payloadHandler o s a = liftIO $ payload a
  status :: IORef State -> IO [Val]
  status s = do
    m <- readIORef s
    (\n -> [Val "localhost" n]) . length . filter isNothing <$> sequence (getExitCode <$> values m)
  -- 処理不可能な引数で落ちる(keyがディレクトリ名として不正)
  -- 通信で落ちる
  payload :: PubSubRequest -> IO ()
  payload a = do
    m <- readIORef s
    x <- (\x -> (x, waitForProcess $ get4 x)) <$> (mkdir o a >>= (\x -> createProcess (uncurry proc (ffmpegCommand o a))) >>= waitForProcess . get4 >>= (\x -> createProcess (uncurry proc (ffmpegCommand2 o a))) >>= (\x -> const x <$> run4 o a))
    writeIORef s $ State $ values m ++ [x]
    return ()

b :: PubSubRequest -> [FilePath] -> IO [(Text, Google.Body)]
b r fs = sequence $ (\x -> (\y -> (fst x, y)) <$> snd x) . (\x -> (T.pack $ (attributesKey . messageAttributes . psrMessage $ r) ++ "/" ++ takeFileName x, Google.sourceBody x)) <$> fs

-- 終わったら削除
-- removeDirectoryRecursive
-- リトライ
-- ロギング
-- タイムアウト
run4 :: Option -> PubSubRequest -> IO ()
run4 config keyy = do
  lgr <- Google.newLogger Google.Debug stdout
  env <- Google.newEnv <&> (Google.envLogger .~ lgr) . (Google.envScopes .~ Storage.storageReadWriteScope)
  bodies <- listFile config keyy >>= b keyy :: IO [(Text, Google.Body)]
  let bucket = O.bucket config
  runResourceT . Google.runGoogle env $
    sequence $ (\x-> Google.upload (Storage.objectsInsert (T.pack bucket) Storage.object' & Storage.oiName ?~ fst x) (snd x)) <$> bodies
  return ()

mkApp :: Option -> IORef State -> IO Application
mkApp o s = return $ serve api (server o s)

run :: IORef State -> Option -> IO ()
run s o = withStdoutLogger $ \apilogger -> do
  let settings =
        setPort (O.port o) $
          setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show (O.port o))) $
            setLogger apilogger defaultSettings
  runSettings settings =<< mkApp o s

