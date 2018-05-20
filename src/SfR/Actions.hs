{-# LANGUAGE OverloadedStrings #-}
module SfR.Actions where

import Control.Monad (liftM)
import Control.Monad.IO.Class
import Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Database.Persist
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Web.Scotty
import Web.Scotty.Cookie

import SfR.Config (callback_url, client_id, secure_cookie, sfr_config)
import SfR.Reddit (identity, savedPosts, savedComments)
import SfR.Reddit.Auth (get_access_token)
import SfR.Reddit.Types
import SfR.Storage (get_or_create_user, get_user_from_session,
                    get_user_saved, normalize_saved, update_saved,
                    userUsername, userSessionKey)
import SfR.Templates.Html (landing_html, view_html)

landing :: ActionM ()
landing = html . TL.pack . renderHtml $ landing_html

login :: ActionM ()
login = do
  client_id <- liftIO $ liftM client_id sfr_config
  callback_url <- liftIO $ liftM callback_url sfr_config
  redirect $ TL.pack (
      "https://www.reddit.com/api/v1/authorize?client_id=" ++
      client_id ++
      "&response_type=code&state=placeholder&redirect_uri=" ++
      callback_url ++
      "&duration=temporary&scope=identity,history"
    )

callback :: ActionM ()
callback = do
  code <- param "code"
  token <- liftIO $ liftM access_token (get_access_token code)

  username <- liftIO $ liftM name (identity token)
  savedPosts <- liftIO $ savedPosts token username ""
  savedComments <- liftIO $ savedComments token username ""

  (userId, user) <- liftIO $ get_or_create_user username
  let savedItems = normalize_saved userId savedPosts savedComments
  liftIO $ update_saved savedItems

  secure_cookie <- liftIO $ liftM secure_cookie sfr_config
  setHeader "Set-Cookie" (TL.pack ("sfr_session=" ++
                                   (userSessionKey user) ++
                                   "; HttpOnly; Path=/; MaxAge=3600" ++
                                   if secure_cookie then "; Secure" else ""))
  redirect "/view"

view :: ActionM ()
view = do
  session <- getCookie "sfr_session"
  case session of
    Nothing  -> redirect "/"
    Just key -> do
      maybeUser <- liftIO $ get_user_from_session (T.unpack key)
      case maybeUser of
        Nothing                   -> redirect "/"
        Just (Entity userId user) -> do
          let username = userUsername user
          saved_items <- liftIO $ get_user_saved username
          subreddit <- (param "subreddit" :: ActionM String) `rescue` (\_ -> return "all")
          html . TL.pack . renderHtml $ view_html saved_items subreddit
