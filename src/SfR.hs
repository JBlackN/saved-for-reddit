{-|
Module     : SfR
Description: Web application's routes
Copyright  : (c) Petr Schmied, 2018
License    : MIT
Maintainer : peter9209@gmail.com
Stability  : stable
Portability: portable

__Saved for Reddit__ downloads and stores saved items from
your [Reddit](https://www.reddit.com) account. You can then browse them with
filtering by subreddit and export them to JSON.

Module defines [Scotty](https://github.com/scotty-web/scotty) web application
and its routes.
-}
{-# LANGUAGE OverloadedStrings #-}
module SfR where

import Web.Scotty
import SfR.Actions (landing, login, callback, view, export, logout)

-- | Saved for Reddit [Scotty](https://github.com/scotty-web/scotty) web application and its routes (all @GET@):
--
-- [@\/@]: Landing page, see 'SfR.Actions.landing'.
-- [@\/auth\/login@]: Redirect to [Reddit](https://www.reddit.com) OAuth page,
--                    see 'SfR.Actions.login'.
-- [@'SfR.Config.callback_path'@]: OAuth callback,
--                                 see 'SfR.Actions.callback'.
-- [@\/view@]: Browsing page, see 'SfR.Actions.view'.
-- [@\/export@]: JSON export & download, see 'SfR.Actions.export'.
-- [@\/logout@]: Logout, see 'SfR.Actions.logout'.
savedForReddit :: String -- ^ OAuth2 callback path.
               -> ScottyM () -- ^ [Scotty](https://github.com/scotty-web/scotty)
                             --   web application.
savedForReddit callback_path = do
  get "/" landing
  get "/auth/login" login
  get (literal callback_path) callback
  get "/view" view
  get "/export" export
  get "/logout" logout
