module Option where

data Option =  Option {
                      port :: Int
                      , dir :: String
                      , bucket :: String
                      , segmentTime :: Int
                      }

