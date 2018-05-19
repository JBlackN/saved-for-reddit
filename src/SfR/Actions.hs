{-# LANGUAGE OverloadedStrings #-}
module SfR.Actions where

import Control.Monad (liftM)
import Control.Monad.IO.Class
import qualified Data.Text.Lazy as TL
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Web.Scotty

import SfR.Config (callback_url, client_id, sfr_config)
import SfR.Reddit (identity, savedPosts, savedComments)
import SfR.Reddit.Auth (get_access_token)
import SfR.Reddit.Types
import SfR.Storage (get_or_create_user, normalize_saved, update_saved)
import SfR.Templates.Html (landing_html)

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

  userId <- liftIO $ get_or_create_user username
  let savedItems = normalize_saved userId savedPosts savedComments
  liftIO $ update_saved savedItems

  html $ "Done"
