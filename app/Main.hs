module Main where

import System.Directory (copyFile, doesFileExist, makeAbsolute)
import Web.Scotty

import SfR (savedForReddit)
import SfR.Config (callback_url, client_id, client_secret, port, sfrConfig)

configHelp :: IO ()
configHelp = do
  config_path <- makeAbsolute "config.yml"
  putStrLn ("config.yml created in " ++ config_path)
  putStrLn ("- open it in your favorite editor and fill in (at least) " ++
            "callback_url, client_id and client_secret")
  putStrLn ("- to obtain client_id and client_secret register the app " ++
            "at https://ssl.reddit.com/prefs/apps/.")

main :: IO ()
main = do
  config_exists <- doesFileExist "config.yml"
  if config_exists then
    (do callback_url <- callback_url <$> sfrConfig
        client_id <- client_id <$> sfrConfig
        client_secret <- client_secret <$> sfrConfig
        if any null [callback_url, client_id, client_secret] then
          configHelp else
          (do port <- port <$> sfrConfig
              scotty port savedForReddit))
    else
    (do copyFile "config.yml.template" "config.yml"
        configHelp)
