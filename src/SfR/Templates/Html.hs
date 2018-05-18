{-# LANGUAGE OverloadedStrings #-}
module SfR.Templates.Html where

import Clay (render)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html5.Attributes.Extra as AE

import SfR.Templates.Css (css)

layout :: Html -> Html
layout content = do
  docType
  html ! lang "en" $ do
    H.head $ do
      meta ! charset "utf-8"
      meta ! name "viewport" ! A.content "width=device-width, initial-scale=1, shrink-to-fit=no"
      link ! rel "stylesheet" ! href "https://stackpath.bootstrapcdn.com/bootstrap/4.1.0/css/bootstrap.min.css" ! integrity "sha384-9gVQ4dYFwwWSjIDZnLEWnxCjeSWFphJiwGPXr1jddIhOegiu1FwO5qRGvFXOdJZ4" ! crossorigin "anonymous"
      H.style (toHtml $ render css) ! type_ "text/css"
      H.title "Saved for Reddit"
    body $ do
      content
      script "" ! src "https://code.jquery.com/jquery-3.3.1.slim.min.js" ! integrity "sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo" ! crossorigin "anonymous"
      script "" ! src "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.0/umd/popper.min.js" ! integrity "sha384-cs/chFZiN24E4KMATLdqdvsezGxaGsi4hLGOzlXwp5UZB1LY//20VyM2taTB4QvJ" ! crossorigin "anonymous"
      script "" ! src "https://stackpath.bootstrapcdn.com/bootstrap/4.1.0/js/bootstrap.min.js" ! integrity "sha384-uefMccjFJAIv6A+rW+L4AHf99KvxDjWSu1z9VI8SKNVmz4sk7buKt/6v9KI65qnm" ! crossorigin "anonymous"

landing_html :: Html
landing_html = layout $ do
  H.div ! class_ "d-flex align-items-center h-100" $ do
    H.div ! class_ "container" $ do
      H.div ! class_ "row text-center" $ do
        H.div ! class_ "col" $ do
          h1 "Saved for Reddit"
          a "Login" ! href "/auth/login" ! class_ "btn btn-outline-primary mt-3" ! role "button"
