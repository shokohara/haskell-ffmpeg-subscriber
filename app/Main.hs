{-# LANGUAGE OverloadedStrings #-}
module Main where

import Api.Payload
import App
import Control.Concurrent
import Data.IORef
import Data.Semigroup ((<>))
import Option
import Options.Applicative
import State

portOpt :: Parser Int
portOpt = option auto (long "port" <> help "Int")

dirOpt :: Parser String
dirOpt = strOption (long "dir" <> help "String")

segmentTimeOpt :: Parser Int
segmentTimeOpt = option auto (long "segmentTime" <> help "Int")

bucketOpt :: Parser String
bucketOpt = strOption (long "bucket" <> help "String")

sample :: Parser Option
sample = Option <$> portOpt <*> dirOpt <*> bucketOpt <*> segmentTimeOpt

opts :: ParserInfo Option
opts = info (sample <**> helper) ( fullDesc
  <> progDesc "Print a greeting for TARGET"
    <> header "hello - a test for optparse-applicative" )

main :: IO ()
main = do
  mvar <- newIORef (State [])
  execParser opts >>= App.run mvar

--main :: IO ()
--main = do
--  mvar <- newIORef []
--  _ <- testFunction mvar
----  _ <- threadDelay 10000
--  m <- readIORef mvar
--  _ <- putStrLn . show $ length m
--  _ <- putStrLn "main end"
--  return ()

