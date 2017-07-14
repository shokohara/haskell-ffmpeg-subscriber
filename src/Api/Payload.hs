{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Api.Payload where

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

--getAllIps :: ClientM (Maybe Val)
--getAllIps = client clientApi
--
--traceIp :: String -> IO (Either ServantErr (Maybe Val))
--traceIp a = do
--  manager <- liftIO $ newManager defaultManagerSettings
--  mapBoth (const err500) id <$> runClientM getAllIps (ClientEnv manager (BaseUrl Http a 80 ""))
--
--try' :: IO a -> IO (Either IOException a)
--try' = try
--
--get4 :: (t2, t1, t, t3) -> t3
--get4 (_, _, _, x) = x
--
--get2 :: (t2, t3, t1, t) -> t3
--get2 (_, x, _, _) = x
--
--getStdOut :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO String
--getStdOut = hGetContents . fromJust . get2
--
---- Keyの重複で例外
--ffmpegCommand :: Option -> PubSubRequest -> (String, [String])
--ffmpegCommand o r = ("ffmpeg", ["-i", [i|${attributesUrl . messageAttributes . psrMessage $ r}|], "-acodec", "copy", "-vcodec", "copy", "-f", "segment", "-segment_time", show . O.segmentTime $ o, "-segment_list", [i|${O.dir $ o}/${unpack . unStorageKey . attributesKey . messageAttributes . psrMessage $ r}/playlist.m3u8|], [i|${O.dir $ o}/${unpack . unStorageKey . attributesKey . messageAttributes . psrMessage $ r}/%d.ts|]])
--
--dirName :: Option -> PubSubRequest -> String
--dirName o r = [i|${O.dir $ o}/${unpack . unStorageKey . attributesKey . messageAttributes . psrMessage $ r}|]
--
--mkdir :: Option -> PubSubRequest -> IO ()
--mkdir o r = createDirectoryIfMissing True $ dirName o r
--
--rm :: Option -> PubSubRequest -> IO ()
--rm o r = removeDirectoryRecursive $ dirName o r
--
--listFile :: Option -> PubSubRequest -> IO [FilePath]
--listFile o r = listFiles $ dirName o r

payloadHandler :: Option -> IORef State -> PubSubRequest -> Handler ()
payloadHandler o s a = liftIO $ payload a

payload :: PubSubRequest -> IO ()
payload a = do
--    _ <- testFunction
--    m <- readIORef s
--    _ <- writeIORef s $ State $ values m ++ [mkDirAndDownload o a]
  return ()

--testFunction :: IO ((), ())
--testFunction :: IORef [(Async (), Async ())] -> IO ()
--testFunction = do
--  pa <- async (putStrLn "start to download" >>= \_ -> putStrLn "downloading" >>= \_ -> threadDelay 1000 >>= \_ -> putStrLn "finish downloading")
--  pb <- async (putStrLn "start to upload" >>= \_ -> threadDelay 1000 >>= \_ -> putStrLn "uploading" >>= \_ -> putStrLn "finish uploading")
--  return (pa, pb)
--testFunction :: IO (Async (), Async ())

--{-# NOINLINE cwd #-}
--cwd :: FilePath
--cwd = unsafePerformIO getCurrentDirectory
--
--o :: Option
--o = Option 3000 [i|${cwd}/tmp|] "shokoharatest" 1
--
--psr :: PubSubRequest
--psr = PubSubRequest (Message (Attributes "1" (StorageKey "thisiskey") "google.com") "" "" "") ""
--
--testFunction s = do
--  m <- readIORef s
----  a <- withAsync (putStrLn "start to download" >>= \_ -> putStrLn "downloading" >>= \_ -> threadDelay 1000 >>= \_ -> putStrLn "finish downloading")
-- -- >>= \pa -> withAsync (putStrLn "start to upload" >>= \_ -> threadDelay 1000 >>= \_ -> putStrLn "uploading" >>= \_ -> putStrLn "finish uploading")
--  --writeIORef s $ [async (putStrLn "start to download" >>= \_ -> putStrLn "downloading" >>= \_ -> threadDelay 1000 >>= \_ -> putStrLn "finish downloading")]
--  writeIORef s $ [async $ mkDirDownloadUpload o psr]
----      return (pa, pb)
--      --return ((), ())
--
--mkDirDownloadUpload :: Option -> PubSubRequest -> IO ()
--mkDirDownloadUpload o a = mkdir o a >>= (\_ -> createProcess (uncurry proc (ffmpegCommand o a))) >>= (\_ -> uploadMovies o a) >>= (\_ -> rm o a)
----mkDirDownloadUpload o a = mkdir o a >>= (\_ -> createProcess (uncurry proc (ffmpegCommand o a))) >>= (\_ -> uploadMovies o a) >>= (\_ -> rm o a)
----mkDirDownloadUpload o a = mkdir o a >>= (\x -> createProcess (uncurry proc (ffmpegCommand o a))) >>= (\x -> (x, uploadMovies o a)) >>= (\x -> const x <$> rm o a)
----mkDirAndDownload :: Option -> PubSubRequest -> (IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle), IO ())
----mkDirAndDownload o a = undefined
----mkDirAndDownload o a = ((\xx -> (fst xx, snd xx >>= uploadMovies o a >>= (\x -> const x <$> rm o a))) $
----  mkdir o a >>=
----    (\x -> createProcess (uncurry proc (ffmpegCommand o a))) >>=
----      (\x -> (x, waitForProcess . get4))
--
--b :: PubSubRequest -> [FilePath] -> IO [(Text, Google.Body)]
--b r fs = sequence $ (\x -> (\y -> (fst x, y)) <$> snd x) . (\x -> (T.pack(unpack (unStorageKey . attributesKey . messageAttributes . psrMessage $ r) ++ "/" ++ takeFileName x), Google.sourceBody x)) <$> fs
--
---- バケットとオブジェクトの命名ガイドライン
---- https://cloud.google.com/storage/docs/naming?hl=ja
---- 処理不可能な引数で落ちる(keyがディレクトリ名として不正)
---- リトライ
---- ロギング
---- タイムアウト
--uploadMovies :: Option -> PubSubRequest -> IO ()
--uploadMovies config keyy = do
--  lgr <- Google.newLogger Google.Debug stdout
--  env <- Google.newEnv <&> (Google.envLogger .~ lgr) . (Google.envScopes .~ Storage.storageReadWriteScope)
--  bodies <- listFile config keyy >>= b keyy :: IO [(Text, Google.Body)]
--  let bucket = O.bucket config
--  runResourceT . Google.runGoogle env $
--    sequence $ (\x-> Google.upload (Storage.objectsInsert (T.pack bucket) Storage.object' & Storage.oiName ?~ fst x) (snd x)) <$> bodies
--  return ()
--
--mkApp :: Option -> IORef State -> IO Application
--mkApp o s = return $ serve api (server o s)
--
--run :: IORef State -> Option -> IO ()
--run s o = withStdoutLogger $ \apilogger -> do
--  let settings =
--        setPort (O.port o) $
--          setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show (O.port o))) $
--            setLogger apilogger defaultSettings
--  runSettings settings =<< mkApp o s

{-# NOINLINE cwd #-}
cwd :: FilePath
cwd = unsafePerformIO getCurrentDirectory

o :: Option
o = Option 3000 [i|${cwd}/tmp|] "shokoharatest" 1

psr :: PubSubRequest
psr = PubSubRequest (Message (Attributes "1" (StorageKey "thisiskey") "google.com") "" "" "") ""

testFunction s = do
  m <- readIORef s
--  a <- withAsync (putStrLn "start to download" >>= \_ -> putStrLn "downloading" >>= \_ -> threadDelay 1000 >>= \_ -> putStrLn "finish downloading")
 -- >>= \pa -> withAsync (putStrLn "start to upload" >>= \_ -> threadDelay 1000 >>= \_ -> putStrLn "uploading" >>= \_ -> putStrLn "finish uploading")
  --writeIORef s $ [async (putStrLn "start to download" >>= \_ -> putStrLn "downloading" >>= \_ -> threadDelay 1000 >>= \_ -> putStrLn "finish downloading")]
  writeIORef s $ m ++ [async $ mkDirDownloadUpload o psr]
--      return (pa, pb)
      --return ((), ())

mkDirDownloadUpload :: Option -> PubSubRequest -> IO ()
mkDirDownloadUpload o a = mkdir o a >>= (\_ -> createProcess (uncurry proc (ffmpegCommand o a))) >>= (\_ -> uploadMovies o a) >>= (\_ -> rm o a)
--mkDirDownloadUpload o a = mkdir o a >>= (\_ -> createProcess (uncurry proc (ffmpegCommand o a))) >>= (\_ -> uploadMovies o a) >>= (\_ -> rm o a)
--mkDirDownloadUpload o a = mkdir o a >>= (\x -> createProcess (uncurry proc (ffmpegCommand o a))) >>= (\x -> (x, uploadMovies o a)) >>= (\x -> const x <$> rm o a)
--mkDirAndDownload :: Option -> PubSubRequest -> (IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle), IO ())
--mkDirAndDownload o a = undefined
--mkDirAndDownload o a = ((\xx -> (fst xx, snd xx >>= uploadMovies o a >>= (\x -> const x <$> rm o a))) $
--  mkdir o a >>=
--    (\x -> createProcess (uncurry proc (ffmpegCommand o a))) >>=
--      (\x -> (x, waitForProcess . get4))

get4 :: (t2, t1, t, t3) -> t3
get4 (_, _, _, x) = x

get2 :: (t2, t3, t1, t) -> t3
get2 (_, x, _, _) = x

getStdOut :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO String
getStdOut = hGetContents . fromJust . get2

-- Keyの重複で例外
ffmpegCommand :: Option -> PubSubRequest -> (String, [String])
ffmpegCommand o r = ("ffmpeg", ["-i", [i|${attributesUrl . messageAttributes . psrMessage $ r}|], "-acodec", "copy", "-vcodec", "copy", "-f", "segment", "-segment_time", show . O.segmentTime $ o, "-segment_list", [i|${O.dir $ o}/${unpack . unStorageKey . attributesKey . messageAttributes . psrMessage $ r}/playlist.m3u8|], [i|${O.dir $ o}/${unpack . unStorageKey . attributesKey . messageAttributes . psrMessage $ r}/%d.ts|]])

dirName :: Option -> PubSubRequest -> String
dirName o r = [i|${O.dir $ o}/${unpack . unStorageKey . attributesKey . messageAttributes . psrMessage $ r}|]

mkdir :: Option -> PubSubRequest -> IO ()
mkdir o r = createDirectoryIfMissing True $ dirName o r

rm :: Option -> PubSubRequest -> IO ()
rm o r = removeDirectoryRecursive $ dirName o r

listFile :: Option -> PubSubRequest -> IO [FilePath]
listFile o r = listFiles $ dirName o r

-- バケットとオブジェクトの命名ガイドライン
-- https://cloud.google.com/storage/docs/naming?hl=ja
-- 処理不可能な引数で落ちる(keyがディレクトリ名として不正)
-- リトライ
-- ロギング
-- タイムアウト
uploadMovies :: Option -> PubSubRequest -> IO ()
uploadMovies config keyy = do
  lgr <- Google.newLogger Google.Debug stdout
  env <- Google.newEnv <&> (Google.envLogger .~ lgr) . (Google.envScopes .~ Storage.storageReadWriteScope)
  bodies <- listFile config keyy >>= b keyy :: IO [(Text, Google.Body)]
  let bucket = O.bucket config
  runResourceT . Google.runGoogle env $
    sequence $ (\x-> Google.upload (Storage.objectsInsert (T.pack bucket) Storage.object' & Storage.oiName ?~ fst x) (snd x)) <$> bodies
  return ()

b :: PubSubRequest -> [FilePath] -> IO [(Text, Google.Body)]
b r fs = sequence $ (\x -> (\y -> (fst x, y)) <$> snd x) . (\x -> (T.pack(unpack (unStorageKey . attributesKey . messageAttributes . psrMessage $ r) ++ "/" ++ takeFileName x), Google.sourceBody x)) <$> fs

