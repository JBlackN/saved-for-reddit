{-# LANGUAGE OverloadedStrings #-}
module SfR where

import Web.Scotty
import SfR.Actions (landing, login, callback)

saved_for_reddit :: ScottyM ()
saved_for_reddit = do
  get "/" $ landing
  get "/auth/login" $ login
  get "/auth/reddit_oauth2/callback" $ callback
  get "/test" $ do
    x <- (param "x") `rescue` (\x -> return "0")
    html $ x
  get "/test2" $ do
    setHeader "X-Test-SfR" "1234"
    redirect "/"
