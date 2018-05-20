{-# LANGUAGE OverloadedStrings #-}
module SfR.Templates.Html where

import Clay (render)
import Control.Monad (forM_)
import Data.Char as C
import Data.List as L
import Data.Set as S hiding (filter)
import Data.String (fromString)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html5.Attributes.Extra as AE

import SfR.Storage
import SfR.Templates.Css (css)
import SfR.Templates.Helpers (show_saved_item, subreddit_filter_option)

layout :: Html -> Html
layout content = do
  docType
  html ! lang "en" $ do
    H.head $ do
      meta ! charset "utf-8"
      meta ! name "viewport" ! A.content "width=device-width, initial-scale=1, shrink-to-fit=no"
      link ! rel "stylesheet" ! href "https://stackpath.bootstrapcdn.com/bootstrap/4.1.0/css/bootstrap.min.css" ! integrity "sha384-9gVQ4dYFwwWSjIDZnLEWnxCjeSWFphJiwGPXr1jddIhOegiu1FwO5qRGvFXOdJZ4" ! crossorigin "anonymous"
      link ! rel "stylesheet" ! href "https://use.fontawesome.com/releases/v5.0.13/css/all.css" ! integrity "sha384-DNOHZ68U8hZfKXOrtjWvjxusGo9WQnrNx2sqG0tfsghAvtVlRW3tvkXWZh58N9jp" ! crossorigin "anonymous"
      H.style (toHtml $ render css) ! type_ "text/css"
      H.title "Saved for Reddit"
    body $ do
      content
      script "" ! src "https://code.jquery.com/jquery-3.3.1.slim.min.js" ! integrity "sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo" ! crossorigin "anonymous"
      script "" ! src "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.0/umd/popper.min.js" ! integrity "sha384-cs/chFZiN24E4KMATLdqdvsezGxaGsi4hLGOzlXwp5UZB1LY//20VyM2taTB4QvJ" ! crossorigin "anonymous"
      script "" ! src "https://stackpath.bootstrapcdn.com/bootstrap/4.1.0/js/bootstrap.min.js" ! integrity "sha384-uefMccjFJAIv6A+rW+L4AHf99KvxDjWSu1z9VI8SKNVmz4sk7buKt/6v9KI65qnm" ! crossorigin "anonymous"

main_menu :: Html
main_menu = do
  nav ! class_ "navbar navbar-expand-lg navbar-light bg-light" $ do
    h1 "Saved for Reddit" ! class_ "navbar-brand mb-0 h1"
    button ! class_ "navbar-toggler" ! type_ "button" ! AE.data_ "toggler" "collapse" ! AE.data_ "target" "#main-menu-items" $ do
      H.span "" ! class_ "navbar-toggler-icon"
    H.div ! class_ "collapse navbar-collapse" ! A.id "main-menu-items" $ do
      ul ! class_ "navbar-nav mr-auto" $ ""
      ul ! class_ "navbar-nav" $ do
        li ! class_ "nav-item" $ do
          a "Export to JSON" ! class_ "nav-link" ! href "/export"

landing_html :: Html
landing_html = layout $ do
  H.div ! class_ "d-flex align-items-center h-100" $ do
    H.div ! class_ "container" $ do
      H.div ! class_ "row text-center" $ do
        H.div ! class_ "col" $ do
          h1 "Saved for Reddit"
          a "Login" ! href "/auth/login" ! class_ "btn btn-outline-primary mt-3" ! role "button"

view_html :: [SavedItem] -> String -> Html
view_html saved_items subreddit = layout $ do
  let filtered_items = case subreddit of
                         "all"     -> saved_items
                         otherwise -> filter process saved_items
                     where process item =
                             subreddit == savedItemSubreddit item
  main_menu
  H.div ! class_ "mt-2 container mb-5" $ do
    H.div ! class_ "row mt-3" $ do
      H.div ! class_ "col" $ do
        H.div ! class_ "card" $ do
          H.div ! class_ "card-body" $ do
            H.div ! class_ "d-flex w-100 justify-content-between align-items-center" $ do
              H.form ! class_ "form-inline" ! action "/view" ! method "get" $ do
                H.label "Subreddit" ! class_ "mr-3" ! for "subreddit"
                select ! class_ "custom-select" ! name "subreddit" ! A.id "subreddit" $ do
                  let subreddits = (L.sortOn (L.map C.toLower) .
                                    S.toList . S.fromList .
                                    L.map (\item -> savedItemSubreddit item)
                                   ) saved_items
                  case subreddit == "all" of
                    False -> option "ALL" ! value "all"
                    True  -> option "ALL" ! value "all" ! selected "selected"
                  forM_ subreddits (subreddit_filter_option subreddit)
                input ! class_ "btn btn-outline-primary ml-3" ! type_ "submit" ! value "Filter"
              small ! class_ "text-muted" $ do
                (string . show . length) filtered_items
                "saved items"
    H.div ! class_ "row" $ do
      H.div ! class_ "col" $ do
        ul ! class_ "list-group mt-3" $ do
          forM_ filtered_items show_saved_item
