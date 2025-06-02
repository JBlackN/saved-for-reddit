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
-- Application uses [Tailwind CSS](https://tailwindcss.com/)
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
        href "https://use.fontawesome.com/releases/v5.0.13/css/all.css" !
        customAttribute "integrity"
          "sha384-DNOHZ68U8hZfKXOrtjWvjxusGo9WQnrNx2sqG0tfsghAvtVlRW3tvkXWZh58N9jp" !
        customAttribute "crossorigin" "anonymous"
      link ! rel "stylesheet" ! href "https://cdn.jsdelivr.net/npm/tailwindcss@2.2.19/dist/tailwind.min.css"
      H.style (toHtml $ render css) ! type_ "text/css"
      H.title "Saved for Reddit"
    body $ do
      content
      H.script ! type_ "text/javascript" $ preEscapedText
        "const btn = document.getElementById('navbar-toggler-button');\
        \const menu = document.getElementById('main-menu-items');\
        \btn.addEventListener('click', () => {\
        \  menu.classList.toggle('hidden');\
        \});"

-- | Web application's main menu.
mainMenu :: Html
mainMenu =
  nav ! class_ "flex lg:flex-row items-center justify-between p-4 bg-gray-100 shadow" $ do
    h1 "Saved for Reddit" ! class_ "text-xl font-bold"
    button ! class_ "lg:hidden p-2 rounded hover:bg-gray-200" ! type_ "button" !
      A.id "navbar-toggler-button" ! -- Added ID for JS
      dataAttribute "toggler" "collapse" !
      dataAttribute "target" "#main-menu-items" $
        H.preEscapedText "<svg class=\"h-6 w-6 fill-current\" viewBox=\"0 0 24 24\"><path fill-rule=\"evenodd\" d=\"M4 5h16a1 1 0 0 1 0 2H4a1 1 0 1 1 0-2zm0 6h16a1 1 0 0 1 0 2H4a1 1 0 0 1 0-2zm0 6h16a1 1 0 0 1 0 2H4a1 1 0 0 1 0-2z\"/></svg>"
    H.div ! class_ "hidden lg:flex flex-col lg:flex-row w-full lg:items-center lg:w-auto" ! A.id "main-menu-items" $ do
      ul ! class_ "flex flex-col lg:flex-row lg:mr-auto" $ "" -- Empty ul for spacing or future items
      ul ! class_ "flex flex-col lg:flex-row space-y-2 lg:space-y-0 lg:space-x-4" $ do
        li $ -- nav-item class removed
          a "Refresh" ! class_ "block lg:inline-block px-2 py-1 hover:text-blue-500" ! href "/sync"
        li $ -- nav-item class removed
          a "Export to JSON" ! class_ "block lg:inline-block px-2 py-1 hover:text-blue-500" ! href "/export"
        li $ -- nav-item class removed
          a "Logout" ! class_ "block lg:inline-block px-2 py-1 hover:text-blue-500" ! href "/logout"

-- | Web application's landing page.
--
-- @See also:@ 'SfR.Actions.landing'.
landingHtml :: Html
landingHtml =
  layout $
    H.div ! class_ "flex items-center h-screen" $ -- d-flex align-items-center h-100
      H.div ! class_ "container mx-auto px-4" $ -- container
        H.div ! class_ "flex justify-center text-center" $ -- row text-center
          H.div ! class_ "w-full" $ do -- col  <<< ADDED do HERE
            h1 "Saved for Reddit"
            a "Login" ! href "/auth/login" !
              class_ "py-2 px-4 font-semibold rounded-lg shadow-md text-blue-700 bg-transparent border border-blue-500 hover:bg-blue-500 hover:text-white hover:border-transparent mt-3" ! -- btn btn-outline-primary mt-3
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
    H.div ! class_ "mt-2 container mx-auto px-4 mb-5" $ do -- mt-2 container mb-5
      H.div ! class_ "flex flex-wrap -mx-2 mt-3" $ -- row mt-3
        H.div ! class_ "w-full px-2" $ -- col
          H.div ! class_ "bg-white shadow-md rounded-lg overflow-hidden" $ -- card
            H.div ! class_ "p-4" $ -- card-body
              H.div !
                class_ "flex w-full justify-between items-center" $ do -- d-flex w-100 justify-content-between align-items-center
                H.form ! class_ "flex items-center space-x-2" ! action "/view" ! method "get" $ do -- form-inline
                  H.label "Subreddit" ! class_ "mr-3" ! for "subreddit" -- mr-3
                  select ! class_ "block appearance-none w-auto bg-white border border-gray-300 hover:border-gray-500 px-4 py-2 pr-8 rounded shadow leading-tight focus:outline-none focus:shadow-outline" ! name "subreddit" ! -- custom-select, w-auto for select
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
                  input ! class_ "py-2 px-4 font-semibold rounded-lg shadow-md text-blue-700 bg-transparent border border-blue-500 hover:bg-blue-500 hover:text-white hover:border-transparent ml-3" ! type_ "submit" ! -- btn btn-outline-primary ml-3
                    value "Filter"
                small ! class_ "text-gray-600 text-sm" $ do -- text-muted
                  (string . show . length) filtered_items
                  "saved items"
      H.div ! class_ "flex flex-wrap -mx-2" $ -- row
        H.div ! class_ "w-full px-2" $ -- col
          ul ! class_ "mt-3 divide-y divide-gray-200" $ -- list-group mt-3
            forM_ filtered_items showSavedItem
