{-|
Module     : SfR.Templates.Helpers
Description: Web application's HTML template helpers
Copyright  : (c) Petr Schmied, 2018
License    : MIT
Maintainer : peter9209@gmail.com
Stability  : stable
Portability: portable

Module defines helper methods for web application's HTML templates (see
"SfR.Templates.Html").
-}
{-# LANGUAGE OverloadedStrings #-}

module SfR.Templates.Helpers where

import Data.Int (Int64)
import Data.String (fromString)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import SfR.Storage

-- | Converts UNIX timestamp to formatted date and time.
formatDatetime :: Int64 -- ^ UNIX timestamp.
               -> String -- ^ Date and time (format: @%d.%m.%Y %H:%M:%S@).
formatDatetime =
  formatTime defaultTimeLocale "%d.%m.%Y %X" .
  posixSecondsToUTCTime . fromIntegral

-- | Generates HTML option for subreddit filter.
--
-- Marks currently selected filter item as such.
subredditFilterOption :: String -- ^ Currently applied filter (subreddit).
                      -> String -- ^ Subreddit to generate HTML option for.
                      -> Html
subredditFilterOption picked subreddit =
  if subreddit == picked then
    option ! (value . fromString) subreddit ! selected "selected" $
      string subreddit
    else option ! (value . fromString) subreddit $ string subreddit

-- | Generates HTML of saved item metadata (author and subreddit).
itemMetadata :: String -- ^ Item's author.
             -> String -- ^ Item's subreddit.
             -> Html
itemMetadata author subreddit = do
  "by"
  a ! class_ "d-inline-block" !
    (href . fromString) ("https://reddit.com/u/" ++ author) !
    target "_blank" $
    string author
  "in"
  a ! class_ "d-inline-block" !
    (href . fromString) ("https://reddit.com/r/" ++ subreddit) !
    target "_blank" $
    string ("r/" ++ subreddit)

-- | Generates HTML list item for saved item.
--
-- Uses item's parent author to determine item's type (posts have no parent).
--
-- @See also:@ 'showSavedPost' and 'showSavedComment'.
showSavedItem :: SavedItem -> Html
showSavedItem item = do
  let parent_author = savedItemParentAuthor item
  case parent_author of
    Nothing -> showSavedPost item
    _       -> showSavedComment item

-- | Generates HTML list item for saved post.
--
-- @See also:@ 'showSavedItem', 'formatDatetime' and 'itemMetadata'.
showSavedPost :: SavedItem -> Html
showSavedPost post =
  li ! class_ "list-group-item flex-column align-items-start" $
    H.div ! class_ "media" $ do
      let maybeThumbnail = savedItemThumbnail post
      case maybeThumbnail of
        Nothing -> ""
        Just thumbnail_url ->
          img ! class_ "mr-3" ! (src . fromString $ thumbnail_url)
      H.div ! class_ "media-body" $ do
        H.div ! class_ "d-flex w-100 justify-content-between" $ do
          h2 ! class_ "mb-1 saved-item-title" $ do
            let link = savedItemLink post
            let permalink = savedItemPermalink post
            a ! class_ "d-inline-block" ! (href . fromString $ permalink) !
              target "_blank" $ toHtml $ savedItemTitle post
            if link == permalink then "" else
              a ! class_ "d-inline-block" ! (href . fromString $ link) !
                target "_blank"
                $ i "" ! class_ "fas fa-external-link-alt ml-1 text-muted"
            H.span ! class_ "badge badge-info ml-1" $
              toHtml $ savedItemScore post
          small $ toHtml $ formatDatetime $ savedItemCreatedUtc post
        small $ do
          let author = savedItemAuthor post
          let subreddit = savedItemSubreddit post
          itemMetadata author subreddit
        let maybeBody = savedItemBody post
        case maybeBody of
          Nothing -> ""
          Just body ->
            p $ preEscapedToHtml body

-- | Generates HTML list item for saved comment.
--
-- @See also:@ 'showSavedItem', 'formatDatetime' and 'itemMetadata'.
showSavedComment :: SavedItem -> Html
showSavedComment comment =
  li ! class_ "list-group-item flex-column align-items-start" $ do
    H.div ! class_ "d-flex w-100 justify-content-between" $ do
      h2 ! class_ "mb-1 saved-item-title" $ do
        let link = savedItemLink comment
        let permalink = savedItemPermalink comment
        a ! class_ "d-inline-block" ! (href . fromString $ link) !
          target "_blank" $ toHtml $ savedItemTitle comment
        if link == permalink then "" else
          a ! class_ "d-inline-block" ! (href . fromString $ permalink) !
            target "_blank"
            $ i "" ! class_ "far fa-comment ml-1 text-muted"
        H.span ! class_ "badge badge-info ml-1" $
          toHtml $ savedItemScore comment
      small $ toHtml $ formatDatetime $ savedItemCreatedUtc comment
    small $ do
      let author = savedItemAuthor comment
      let subreddit = savedItemSubreddit comment
      itemMetadata author subreddit
    let maybeBody = savedItemBody comment
    case maybeBody of
      Nothing -> ""
      Just body ->
        p $ preEscapedToHtml body
    small $ do
      let maybeCommentAuthor = savedItemParentAuthor comment
      case maybeCommentAuthor of
        Nothing -> ""
        Just author -> do
          "comment by"
          a ! class_ "d-inline-block" !
            (href . fromString) ("https://reddit.com/u/" ++ author) !
            target "_blank" $
            string author
