module Main where

import Control.Monad (liftM)
import System.Directory (copyFile, doesFileExist, makeAbsolute)
import Web.Scotty

import SfR (saved_for_reddit)
import SfR.Config (callback_url, client_id, client_secret, port, sfr_config)

config_help :: IO ()
config_help = do
  config_path <- makeAbsolute "config.yml"
  putStrLn ("config.yml created in " ++ config_path)
  putStrLn ("- open it in your favorite editor and fill in (at least) " ++
            "callback_url, client_id and client_secret")
  putStrLn ("- to obtain client_id and client_secret register the app " ++
            "at https://ssl.reddit.com/prefs/apps/.")

main :: IO ()
main = do
  config_exists <- doesFileExist "config.yml"
  case config_exists of
    False -> do
      copyFile "config.yml.template" "config.yml"
      config_help
    True -> do
      callback_url <- liftM callback_url sfr_config
      client_id <- liftM client_id sfr_config
      client_secret <- liftM client_secret sfr_config
      case any null [callback_url, client_id, client_secret] of
        True  -> config_help
        False -> do
          port <- liftM port sfr_config
          scotty port saved_for_reddit
