{-# LANGUAGE OverloadedStrings #-}
module Main where

import App
import Options.Applicative
import Data.Semigroup ((<>))
import Option
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import STMContainers.Map
import Control.Concurrent.MVar

portOpt :: Parser Int
portOpt = option auto (long "port" <> help "Int")

dirOpt :: Parser String
dirOpt = option auto (long "dir" <> help "String")

sample :: Parser Option
sample = Option <$> portOpt <*> dirOpt

opts :: ParserInfo Option
opts = info (sample <**> helper) ( fullDesc
  <> progDesc "Print a greeting for TARGET"
      <> header "hello - a test for optparse-applicative" )

--x :: IO StateVar
--x = do
--  return (StateVar sem)

main :: IO ()
main = do
  --state <- atomically $ (new :: STM (Map Integer Integer))
  mvar <- newMVar (State [])
--  state <- atomically $ newTVar (0, 0)
--  let config = Configg { myState = state }
  execParser opts >>= App.run mvar

