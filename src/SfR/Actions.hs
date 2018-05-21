{-# LANGUAGE OverloadedStrings #-}
module SfR.Actions where

import Control.Monad.IO.Class
import Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Database.Persist
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Web.Scotty
import Web.Scotty.Cookie

import SfR.Config (callback_url, client_id, secure_cookie, sfrConfig)
import SfR.Reddit (identity, savedPosts, savedComments)
import SfR.Reddit.Auth (getAccessToken)
import SfR.Reddit.Types
import SfR.Storage (getOrCreateUser, getUserFromSession,
                    getUserSaved, normalizeSaved, updateSaved,
                    userUsername, userSessionKey)
import SfR.Templates.Html (landingHtml, viewHtml)

landing :: ActionM ()
landing = html . TL.pack . renderHtml $ landingHtml

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

callback :: ActionM ()
callback = do
  code <- param "code"
  token <- liftIO $ access_token <$> getAccessToken code

  username <- liftIO $ name <$> identity token
  savedPosts <- liftIO $ savedPosts token username ""
  savedComments <- liftIO $ savedComments token username ""

  (userId, user) <- liftIO $ getOrCreateUser username
  let savedItems = normalizeSaved userId savedPosts savedComments
  liftIO $ updateSaved savedItems

  secure_cookie <- liftIO $ secure_cookie <$> sfrConfig
  setHeader "Set-Cookie" (TL.pack ("sfr_session=" ++
                                   userSessionKey user ++
                                   "; HttpOnly; Path=/; MaxAge=3600" ++
                                   if secure_cookie then "; Secure" else ""))
  redirect "/view"

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

logout :: ActionM ()
logout = do
  setHeader "Set-Cookie" (TL.pack ("sfr_session=null; HttpOnly; Path=/; " ++
                                   "MaxAge=0; " ++
                                   "Expires=Thu, 01 Jan 1970 00:00:00 GMT"))
  redirect "/"
