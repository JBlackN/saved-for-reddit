{-|
Module     : SfR.Actions
Description: Web application's actions
Copyright  : (c) Petr Schmied, 2018
License    : MIT
Maintainer : peter9209@gmail.com
Stability  : stable
Portability: portable

Module defines [Scotty](https://github.com/scotty-web/scotty) web application's
actions for corresponding routes (see "SfR").
-}
{-# LANGUAGE OverloadedStrings #-}
module SfR.Actions where

import Control.Monad.IO.Class
import Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Database.Persist
import Database.Persist.Sql (fromSqlKey)
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Web.Scotty
import Web.Scotty.Cookie

import SfR.Config (callback_url, client_id, secure_cookie, sfrConfig)
import SfR.Reddit (identity, savedPosts, savedComments)
import SfR.Reddit.Auth (getAccessToken)
import SfR.Reddit.Types
import SfR.Storage (getOrCreateUser, getUserFromSession,
                    getUserSaved, normalizeSaved, updateSaved,
                    userUsername, userSessionKey, userAccessToken)
import SfR.Templates.Html (landingHtml, viewHtml)

-- | Landing page action.
--
-- See 'SfR.Templates.Html.landingHtml'.
landing :: ActionM ()
landing = html . TL.pack . renderHtml $ landingHtml

-- | Login action.
--
-- Loads @client_id@ and @callback_url@ from the config file (see "SfR.Config")
-- and redirects to [Reddit](https://www.reddit.com) OAuth authorization page.
login :: ActionM ()
login = do
  client_id <- liftIO $ client_id <$> sfrConfig
  callback_url <- liftIO $ callback_url <$> sfrConfig
  redirect $ TL.pack (
      "https://www.reddit.com/api/v1/authorize?client_id=" ++
      client_id ++
      "&response_type=code&state=placeholder&redirect_uri=" ++
      callback_url ++
      "&duration=temporary&scope=identity,history"
    )

-- | OAuth2 callback action.
--
-- (1) Receives authorization code from [Reddit](https://www.reddit.com) and
--     uses it to obtain user's access token (see
--     'SfR.Reddit.Auth.getAccessToken').
-- (2) Gets user's identity (username), see 'SfR.Reddit.identity'.
-- (3) Registers the user within the app if needed (see
--     'SfR.Storage.getOrCreateUser').
-- (4) Logs the user in (sends session cookie) and redirects to sync action
--     (see 'sync').
callback :: ActionM ()
callback = do
  code <- param "code"
  token <- liftIO $ access_token <$> getAccessToken code

  username <- liftIO $ name <$> identity token
  (_, user) <- liftIO $ getOrCreateUser username token

  secure_cookie <- liftIO $ secure_cookie <$> sfrConfig
  setHeader "Set-Cookie" (TL.pack ("sfr_session=" ++
                                   userSessionKey user ++
                                   "; HttpOnly; Path=/; MaxAge=3600" ++
                                   if secure_cookie then "; Secure" else ""))
  redirect "/sync"

-- | Sync action (refreshes saved items from [Reddit](https://www.reddit.com)).
--
-- (1) Attempts to get user from session (see 'SfR.Storage.getUserFromSession').
--     Redirects to 'login' if unsuccesful.
-- (2) Gets user's saved posts and comments
--     from [Reddit](https://www.reddit.com) (see 'SfR.Reddit.savedPosts',
--     'SfR.Reddit.savedComments').
-- (3) Updates user's saved items in the database (see
--     'SfR.Storage.normalizeSaved', 'SfR.Storage.updateSaved').
-- (4) Redirects user to the browsing view (see 'view').
--
-- @Note:@ [Reddit](https://www.reddit.com) posts and comments are obtained
-- separately due to differences in their data representations. Application
-- stores them in normalized form (see 'SfR.Storage.normalizeSaved').
sync :: ActionM ()
sync = do
  session <- getCookie "sfr_session"
  case session of
    Nothing  -> redirect "/"
    Just key -> do
      maybeUser <- liftIO $ getUserFromSession (T.unpack key)
      case maybeUser of
        Nothing                   -> redirect "/"
        Just (Entity userId user) -> do
          let username = userUsername user
          let accessToken = userAccessToken user
          case accessToken of
            Nothing    -> redirect "/"
            Just token -> do
              savedPosts <- liftIO $ savedPosts token username ""
              savedComments <- liftIO $ savedComments token username ""
              let savedItems =
                    normalizeSaved (fromSqlKey userId) savedPosts savedComments
              liftIO $ updateSaved savedItems
              redirect "/view"

-- | Browsing view action.
--
-- (1) Attempts to get user from session (see 'SfR.Storage.getUserFromSession').
--     Redirects to 'login' if unsuccesful.
-- (2) Gets user's saved items (see 'SfR.Storage.getUserSaved') and current
--     subreddit filter's value (default: @all@).
-- (3) Renders the view (see 'SfR.Templates.Html.viewHtml').
view :: ActionM ()
view = do
  session <- getCookie "sfr_session"
  case session of
    Nothing  -> redirect "/"
    Just key -> do
      maybeUser <- liftIO $ getUserFromSession (T.unpack key)
      case maybeUser of
        Nothing                   -> redirect "/"
        Just (Entity userId user) -> do
          let username = userUsername user
          saved_items <- liftIO $ getUserSaved username
          subreddit <-
            (param "subreddit" :: ActionM String) `rescue` (\_ -> return "all")
          html . TL.pack . renderHtml $ viewHtml saved_items subreddit

-- | JSON export view.
--
-- (1) Attempts to get user from session (see 'SfR.Storage.getUserFromSession').
--     Redirects to 'login' if unsuccesful.
-- (2) Gets user's saved items (see 'SfR.Storage.getUserSaved').
-- (3) Renders JSON file and sends it for download.
export :: ActionM ()
export = do
  session <- getCookie "sfr_session"
  case session of
    Nothing  -> redirect "/"
    Just key -> do
      maybeUser <- liftIO $ getUserFromSession (T.unpack key)
      case maybeUser of
        Nothing                   -> redirect "/"
        Just (Entity userId user) -> do
          let username = userUsername user
          saved_items <- liftIO $ getUserSaved username
          setHeader "Content-Disposition" "attachment; filename=\"export.json\""
          json saved_items

-- | Logout action.
--
-- Clears user's session and redirects to 'login'.
logout :: ActionM ()
logout = do
  setHeader "Set-Cookie" (TL.pack ("sfr_session=null; HttpOnly; Path=/; " ++
                                   "MaxAge=0; " ++
                                   "Expires=Thu, 01 Jan 1970 00:00:00 GMT"))
  redirect "/"
