{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

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
import           Data.Text (Text, unpack, pack, append, replace)
import Data.UUID
import           Debug.Trace
import qualified Network.Google as Google
import Network.Google.Storage
import qualified Network.Google.Storage as Storage
import           Network.HTTP.Client hiding (Proxy, RequestBody)
import Network.HTTP.Media ((//))
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger       (withStdoutLogger)
import           Option
import           Option (Option)
import qualified Option as O
import           Prelude hiding (lookup)
import           Servant
import State
import           System.Directory
import           System.Directory.Extra
import System.Exit
import           System.FilePath.Posix
import           System.IO
import           System.IO.Unsafe
import           System.Process hiding (cwd, env)
import System.Random

payloadHandler :: Option -> IORef State -> PubSubRequest -> Handler ()
payloadHandler o s p = liftIO $ payload o p s

payload :: Option -> PubSubRequest -> IORef State -> IO ()
payload o a s = do
  i <- mkDirDownloadUploadAsync o a
  modifyIORef' s (\m -> State $ values m ++ [i])

newUUID :: IO UUID
newUUID = randomIO

mkDirDownloadUpload :: Option -> PubSubRequest -> IO ()
mkDirDownloadUpload o a = do
  mkdir o a
  createProcess (uncurry proc $ ffmpegCommand o a) >>= waitForProcess . get4
  uploadMovies o a
  rm o a

-- Keyの重複で例外
ffmpegCommand :: Option -> PubSubRequest -> (String, [String])
ffmpegCommand o r = ("ffmpeg", ["-i", [i|${attributesUrl . messageAttributes . psrMessage $ r}|], "-acodec", "copy", "-vcodec", "copy", "-f", "segment", "-segment_time", show . O.segmentTime $ o, "-segment_list", [i|${O.dir $ o}/${unpack . unStorageKey . attributesKey . messageAttributes . psrMessage $ r}/playlist.m3u8|], [i|${O.dir $ o}/${unpack . unStorageKey . attributesKey . messageAttributes . psrMessage $ r}/%d.ts|]])

mkdir :: Option -> PubSubRequest -> IO ()
mkdir o r = createDirectoryIfMissing True $ dirName o r

rm :: Option -> PubSubRequest -> IO ()
rm o r = removeDirectoryRecursive $ dirName o r

uploadMovies :: Option -> PubSubRequest -> IO ()
uploadMovies config keyy = do
  uuid <- newUUID
  _ <- createTmpFile (pack . O.bucket $ config) keyy uuid
  lgr <- Google.newLogger Google.Info stdout
  env <- Google.newEnv <&> (Google.envLogger .~ lgr) . (Google.envScopes .~ Storage.storageReadWriteScope)
  bodies <- listFile config keyy >>= function1 keyy uuid :: IO [(Text, Google.Body)]
  runResourceT . Google.runGoogle env $
    sequence $ (\x-> Google.upload (Storage.objectsInsert (T.pack $ O.bucket config) Storage.object' & Storage.oiName ?~ fst x) (snd x)) <$> bodies
  return ()

mkDirDownloadUploadAsync :: Option -> PubSubRequest -> IO (Async ())
mkDirDownloadUploadAsync o a = async $ mkDirDownloadUpload o a

get4 :: (t2, t1, t, t3) -> t3
get4 (_, _, _, x) = x

get2 :: (t2, t3, t1, t) -> t3
get2 (_, x, _, _) = x

getStdOut :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO String
getStdOut = hGetContents . fromJust . get2

dirName :: Option -> PubSubRequest -> String
dirName o r = [i|${O.dir $ o}/${unpack . unStorageKey . attributesKey . messageAttributes . psrMessage $ r}|]

listFile :: Option -> PubSubRequest -> IO [FilePath]
listFile o r = listFiles $ dirName o r

-- バケットとオブジェクトの命名ガイドライン
-- https://cloud.google.com/storage/docs/naming?hl=ja
-- 処理不可能な引数で落ちる(keyがディレクトリ名として不正)
-- リトライ
-- ロギング
-- タイムアウト
function1 :: PubSubRequest -> UUID -> [FilePath] -> IO [(Text, Google.Body)]
function1 r uuid fs = sequence $ (\x -> (\y -> (fst x, y)) <$> snd x) . (\x -> (name r uuid x, Google.sourceBody x)) <$> fs

bucketDirectoryName :: PubSubRequest -> UUID -> Text
bucketDirectoryName r uuid = pack $ "uuid-" ++ show uuid ++ "-" ++ unpack (unStorageKey . attributesKey . messageAttributes . psrMessage $ r) ++ "/"

name :: PubSubRequest -> UUID -> [Char] -> Text
name r uuid x = bucketDirectoryName r uuid `append` pack (takeFileName x)

tmpFileName :: PubSubRequest -> UUID -> Text
tmpFileName r a = bucketDirectoryName r a `append` pack "tmpfile"

createTmpFile :: Text -> PubSubRequest -> UUID -> IO Object
createTmpFile bucketName r uuid = do
  lgr <- Google.newLogger Google.Info stdout
  env <- Google.newEnv <&> (Google.envLogger .~ lgr) . (Google.envScopes .~ Storage.storageReadWriteScope)
  runResourceT . Google.runGoogle env $
    Google.upload (Storage.objectsInsert bucketName Storage.object' & Storage.oiName ?~ tmpFileName r uuid) emptyFile

deleteTmpFile :: Text -> PubSubRequest -> UUID -> IO ()
deleteTmpFile bucketName r uuid = do
  lgr <- Google.newLogger Google.Info stdout
  env <- Google.newEnv <&> (Google.envLogger .~ lgr) . (Google.envScopes .~ Storage.storageReadWriteScope)
  runResourceT . Google.runGoogle env $
    Google.send (Storage.objectsDelete bucketName (replace "/" "%2F" $ tmpFileName r uuid))

listTmpFile :: Text -> PubSubRequest -> UUID -> IO Storage.Objects
listTmpFile bucketName r uuid = do
  lgr <- Google.newLogger Google.Info stdout
  env <- Google.newEnv <&> (Google.envLogger .~ lgr) . (Google.envScopes .~ Storage.storageReadWriteScope)
  runResourceT . Google.runGoogle env $
    Google.send (Storage.objectsList bucketName & Storage.olPrefix .~ Just (tmpFileName r uuid) & Storage.olMaxResults .~ Just 1000)

emptyFile :: Google.Body
emptyFile = Google.Body ("text" // "plain") (RequestBodyBS "")

