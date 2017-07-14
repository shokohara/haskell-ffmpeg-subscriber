module Option where

import Data.Text (Text)

data Option =  Option {
                      port :: Int
                      , dir :: String
                      , bucket :: String
                      , segmentTime :: Int
                      }

