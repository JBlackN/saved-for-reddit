{-# LANGUAGE OverloadedStrings #-}
module SfR.Templates.Helpers where

import Data.Int (Int64)
import Data.String (fromString)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import SfR.Storage

format_datetime :: Int64 -> String
format_datetime = formatTime defaultTimeLocale "%d.%m.%Y %X" .
                  posixSecondsToUTCTime . fromIntegral

show_saved_item :: SavedItem -> Html
show_saved_item item = do
  let parent_author = savedItemParentAuthor item
  case parent_author of
    Nothing   -> show_saved_post item
    otherwise -> show_saved_comment item

show_saved_post :: SavedItem -> Html
show_saved_post post = do
  li ! class_ "list-group-item list-group-item-action flex-column align-items-start" $ do
    H.div ! class_ "media" $ do
      let maybeThumbnail = savedItemThumbnail post
      case maybeThumbnail of
        Nothing            -> ""
        Just thumbnail_url -> do
          img ! class_ "mr-3" ! (src . fromString $ thumbnail_url)
      H.div ! class_ "media-body" $ do
        H.div ! class_ "d-flex w-100 justify-content-between" $ do
          h2 ! class_ "mb-1 saved-item-title" $ do
            let link = savedItemLink post
            let permalink = savedItemPermalink post
            a ! class_ "d-inline-block" ! (href . fromString $ permalink) ! target "_blank" $ do
              toHtml $ (savedItemTitle post)
            case link == permalink of
              True  -> ""
              False -> do
                a ! class_ "d-inline-block" ! (href . fromString $ link) ! target "_blank" $ do
                  i "" ! class_ "fas fa-external-link-alt ml-1 text-muted"
            H.span ! class_ "badge badge-info ml-1" $ do
              toHtml $ (savedItemScore post)
          small $ do
            toHtml $ format_datetime $ (savedItemCreatedUtc post)
        small $ do
          let author = savedItemAuthor post
          let subreddit = savedItemSubreddit post
          "by"
          a ! class_ "d-inline-block" ! (href . fromString) ("https://reddit.com/u/" ++ author) ! target "_blank" $ string $ author
          "in"
          a ! class_ "d-inline-block" ! (href . fromString) ("https://reddit.com/r/" ++ subreddit) ! target "_blank" $ string $ ("r/" ++ subreddit)
        let maybeBody = savedItemBody post
        case maybeBody of
          Nothing -> ""
          Just body -> do
            p $ do
              preEscapedToHtml $ body

show_saved_comment :: SavedItem -> Html
show_saved_comment comment = do
  li ! class_ "list-group-item list-group-item-action flex-column align-items-start" $ do
    H.div ! class_ "d-flex w-100 justify-content-between" $ do
      h2 ! class_ "mb-1 saved-item-title" $ do
        let link = savedItemLink comment
        let permalink = savedItemPermalink comment
        a ! class_ "d-inline-block" ! (href . fromString $ link) ! target "_blank" $ do
          toHtml $ (savedItemTitle comment)
        case link == permalink of
          True  -> ""
          False -> do
            a ! class_ "d-inline-block" ! (href . fromString $ permalink) ! target "_blank" $ do
              i "" ! class_ "far fa-comment ml-1 text-muted"
        H.span ! class_ "badge badge-info ml-1" $ do
          toHtml $ (savedItemScore comment)
      small $ do
        toHtml $ format_datetime $ (savedItemCreatedUtc comment)
    small $ do
      let author = savedItemAuthor comment
      let subreddit = savedItemSubreddit comment
      "by"
      a ! class_ "d-inline-block" ! (href . fromString) ("https://reddit.com/u/" ++ author) ! target "_blank" $ string $ author
      "in"
      a ! class_ "d-inline-block" ! (href . fromString) ("https://reddit.com/r/" ++ subreddit) ! target "_blank" $ string $ ("r/" ++ subreddit)
    let maybeBody = savedItemBody comment
    case maybeBody of
      Nothing -> ""
      Just body -> do
        p $ do
          preEscapedToHtml $ body
    small $ do
      let maybeCommentAuthor = savedItemParentAuthor comment
      case maybeCommentAuthor of
        Nothing -> ""
        Just author -> do
          "comment by"
          a ! class_ "d-inline-block" ! (href . fromString) ("https://reddit.com/u/" ++ author) ! target "_blank" $ string $ author
