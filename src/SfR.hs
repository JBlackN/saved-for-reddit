{-# LANGUAGE OverloadedStrings #-}
module SfR where

import Web.Scotty
import SfR.Actions (landing, login, callback, view, export, logout)

savedForReddit :: ScottyM ()
savedForReddit = do
  get "/" landing
  get "/auth/login" login
  get "/auth/reddit_oauth2/callback" callback
  get "/view" view
  get "/export" export
  get "/logout" logout
