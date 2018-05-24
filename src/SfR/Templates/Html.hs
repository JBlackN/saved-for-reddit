{-|
Module     : SfR.Templates.Html
Description: Web application's HTML templates
Copyright  : (c) Petr Schmied, 2018
License    : MIT
Maintainer : peter9209@gmail.com
Stability  : stable
Portability: portable

Module defines web application's HTML templates.

@See also:@ "SfR.Templates.Helpers", "SfR.Templates.Css".
-}
{-# LANGUAGE OverloadedStrings #-}

module SfR.Templates.Html where

import Clay (render)
import Control.Monad (forM_)
import Data.Char as C
import Data.List as L
import Data.Set as S hiding (filter)
import Data.String (fromString)
import Text.Blaze
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import SfR.Storage
import SfR.Templates.Css (css)
import SfR.Templates.Helpers (showSavedItem, subredditFilterOption)

-- | Web application's layout.
--
-- Application uses [Bootstrap](https://getbootstrap.com/)
-- and [Font Awesome](https://fontawesome.com/).
--
-- See "SfR.Templates.Css" for additional CSS stylesheets.
layout :: Html -> Html
layout content = do
  docType
  html ! lang "en" $ do
    H.head $ do
      meta ! charset "utf-8"
      meta ! name "viewport" !
        A.content "width=device-width, initial-scale=1, shrink-to-fit=no"
      link ! rel "stylesheet" !
        href
          "https://stackpath.bootstrapcdn.com/bootstrap/4.1.0/css/bootstrap.min.css" !
        customAttribute "integrity"
          "sha384-9gVQ4dYFwwWSjIDZnLEWnxCjeSWFphJiwGPXr1jddIhOegiu1FwO5qRGvFXOdJZ4" !
        customAttribute "crossorigin" "anonymous"
      link ! rel "stylesheet" !
        href "https://use.fontawesome.com/releases/v5.0.13/css/all.css" !
        customAttribute "integrity"
          "sha384-DNOHZ68U8hZfKXOrtjWvjxusGo9WQnrNx2sqG0tfsghAvtVlRW3tvkXWZh58N9jp" !
        customAttribute "crossorigin" "anonymous"
      H.style (toHtml $ render css) ! type_ "text/css"
      H.title "Saved for Reddit"
    body $ do
      content
      script "" ! src "https://code.jquery.com/jquery-3.3.1.slim.min.js" !
        customAttribute "integrity"
          "sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo" !
        customAttribute "crossorigin" "anonymous"
      script "" !
        src
          "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.0/umd/popper.min.js" !
        customAttribute "integrity"
          "sha384-cs/chFZiN24E4KMATLdqdvsezGxaGsi4hLGOzlXwp5UZB1LY//20VyM2taTB4QvJ" !
        customAttribute "crossorigin" "anonymous"
      script "" !
        src
          "https://stackpath.bootstrapcdn.com/bootstrap/4.1.0/js/bootstrap.min.js" !
        customAttribute "integrity"
          "sha384-uefMccjFJAIv6A+rW+L4AHf99KvxDjWSu1z9VI8SKNVmz4sk7buKt/6v9KI65qnm" !
        customAttribute "crossorigin" "anonymous"

-- | Web application's main menu.
mainMenu :: Html
mainMenu =
  nav ! class_ "navbar navbar-expand-lg navbar-light bg-light" $ do
    h1 "Saved for Reddit" ! class_ "navbar-brand mb-0 h1"
    button ! class_ "navbar-toggler" ! type_ "button" !
      dataAttribute "toggler" "collapse" !
      dataAttribute "target" "#main-menu-items" $
        H.span "" ! class_ "navbar-toggler-icon"
    H.div ! class_ "collapse navbar-collapse" ! A.id "main-menu-items" $ do
      ul ! class_ "navbar-nav mr-auto" $ ""
      ul ! class_ "navbar-nav" $ do
        li ! class_ "nav-item" $
          a "Refresh" ! class_ "nav-link" ! href "/sync"
        li ! class_ "nav-item" $
          a "Export to JSON" ! class_ "nav-link" ! href "/export"
        li ! class_ "nav-item" $
          a "Logout" ! class_ "nav-link" ! href "/logout"

-- | Web application's landing page.
--
-- @See also:@ 'SfR.Actions.landing'.
landingHtml :: Html
landingHtml =
  layout $
    H.div ! class_ "d-flex align-items-center h-100" $
      H.div ! class_ "container" $
        H.div ! class_ "row text-center" $
          H.div ! class_ "col" $ do
            h1 "Saved for Reddit"
            a "Login" ! href "/auth/login" !
              class_ "btn btn-outline-primary mt-3" !
              customAttribute "role" "button"

-- | Web application's browsing view page.
--
-- Displays saved items optionally filtered by subreddit.
--
-- @See also:@ 'SfR.Actions.view',
--             'SfR.Templates.Helpers.subredditFilterOption',
--             'SfR.Templates.Helpers.showSavedItem'.
viewHtml :: [SavedItem] -- ^ Saved items to display.
         -> String -- ^ Subreddit to filter saved items by.
         -> Html
viewHtml saved_items subreddit =
  layout $ do
    let filtered_items =
          case subreddit of
            "all" -> saved_items
            _     -> filter process saved_items
          where
            process item = subreddit == savedItemSubreddit item
    mainMenu
    H.div ! class_ "mt-2 container mb-5" $ do
      H.div ! class_ "row mt-3" $
        H.div ! class_ "col" $
          H.div ! class_ "card" $
            H.div ! class_ "card-body" $
              H.div !
                class_ "d-flex w-100 justify-content-between align-items-center" $ do
                H.form ! class_ "form-inline" ! action "/view" ! method "get" $ do
                  H.label "Subreddit" ! class_ "mr-3" ! for "subreddit"
                  select ! class_ "custom-select" ! name "subreddit" !
                    A.id "subreddit" $ do
                    let subreddits =
                          (L.sortOn (L.map C.toLower) .
                           S.toList .
                           S.fromList . L.map savedItemSubreddit
                          ) saved_items
                    if subreddit == "all" then
                      option "ALL" ! value "all" ! selected "selected" else
                      option "ALL" ! value "all"
                    forM_ subreddits (subredditFilterOption subreddit)
                  input ! class_ "btn btn-outline-primary ml-3" ! type_ "submit" !
                    value "Filter"
                small ! class_ "text-muted" $ do
                  (string . show . length) filtered_items
                  "saved items"
      H.div ! class_ "row" $
        H.div ! class_ "col" $
          ul ! class_ "list-group mt-3" $
            forM_ filtered_items showSavedItem
