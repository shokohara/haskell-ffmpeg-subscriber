{-# LANGUAGE OverloadedStrings #-}
module Main where

import App
import Options.Applicative
import Data.Semigroup ((<>))
import Option

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

type State = [IO ()]

main :: IO ()
main = execParser opts >>= App.run

