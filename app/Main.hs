{-# LANGUAGE OverloadedStrings #-}
module Main where

import App
import Options.Applicative
import Data.Semigroup ((<>))
import Option
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import STMContainers.Map

gbOpt :: Parser Int
gbOpt = option auto (long "port" <> help "Int")

percentOpt :: Parser Float
percentOpt = option auto (long "percent" <> help "Float")

sample :: Parser Option
sample = Option <$> gbOpt

opts :: ParserInfo Option
opts = info (sample <**> helper) ( fullDesc
  <> progDesc "Print a greeting for TARGET"
      <> header "hello - a test for optparse-applicative" )

main :: IO ()
main = do
  --state <- atomically $ (new :: STM (Map Integer Integer))
  state <- atomically $ newTVar (0, 0)
  let config = Configg { myState = state }
  execParser opts >>= App.run

