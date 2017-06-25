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
  state <- atomically $ (new :: STM (Map Integer Integer))
  execParser opts >>= App.run

