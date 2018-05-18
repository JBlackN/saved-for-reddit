module Main where

import Control.Monad (liftM)
import Web.Scotty

import SfR (saved_for_reddit)
import SfR.Config (port, sfr_config)

main :: IO ()
main = do
  port <- liftM port sfr_config
  scotty port saved_for_reddit
