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
-- Note: This helper's classes might need review if select's styling changed significantly.
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
  "by "
  a ! class_ "inline-block hover:underline text-blue-600 mx-1" !
    (href . fromString) ("https://reddit.com/u/" ++ author) !
    target "_blank" $
    string author
  " in "
  a ! class_ "inline-block hover:underline text-blue-600 mx-1" !
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
  li ! class_ "p-4" $ -- list-group-item replaced, relies on parent ul's divide for borders
    H.div ! class_ "flex items-start" $ do -- media class replaced
      let maybeThumbnail = savedItemThumbnail post
      case maybeThumbnail of
        Nothing -> ""
        Just thumbnail_url ->
          img ! class_ "w-24 h-24 object-cover mr-3 rounded" ! (src . fromString $ thumbnail_url) -- mr-3 kept, added size and style
      H.div ! class_ "flex-1" $ do -- media-body class replaced
        H.div ! class_ "flex w-full justify-between items-center" $ do -- d-flex w-100 justify-content-between, added items-center
          h2 ! class_ "mb-1 text-xl font-semibold" $ do -- saved-item-title & mb-1
            let link = savedItemLink post
            let permalink = savedItemPermalink post
            a ! class_ "inline-block hover:underline" ! (href . fromString $ permalink) ! -- d-inline-block
              target "_blank" $ toHtml $ savedItemTitle post
            if link == permalink then "" else
              a ! class_ "inline-block ml-2 text-gray-400 hover:text-gray-600" ! (href . fromString $ link) ! -- d-inline-block
                target "_blank"
                $ i "" ! class_ "fas fa-external-link-alt" -- ml-1 text-muted removed from here, handled by parent a
            H.span ! class_ "inline-block px-2 py-0.5 text-xs font-semibold rounded bg-blue-200 text-blue-800 ml-2" $ -- badge badge-info ml-1 (py adjusted)
              toHtml $ savedItemScore post
          small ! class_ "text-sm text-gray-500" $ toHtml $ formatDatetime $ savedItemCreatedUtc post -- text-muted equivalent
        H.div ! class_ "text-sm text-gray-600 mt-1" $ do -- small wrapper for metadata
          let author = savedItemAuthor post
          let subreddit = savedItemSubreddit post
          itemMetadata author subreddit
        let maybeBody = savedItemBody post
        case maybeBody of
          Nothing -> ""
          Just body ->
            H.div ! class_ "prose prose-sm mt-2" $ preEscapedToHtml body -- Added prose for rich text styling

-- | Generates HTML list item for saved comment.
--
-- @See also:@ 'showSavedItem', 'formatDatetime' and 'itemMetadata'.
showSavedComment :: SavedItem -> Html
showSavedComment comment =
  li ! class_ "p-4" $ do -- list-group-item replaced
    H.div ! class_ "flex w-full justify-between items-center" $ do -- d-flex w-100 justify-content-between, added items-center
      h2 ! class_ "mb-1 text-xl font-semibold" $ do -- saved-item-title & mb-1
        let link = savedItemLink comment
        let permalink = savedItemPermalink comment
        a ! class_ "inline-block hover:underline" ! (href . fromString $ link) ! -- d-inline-block
          target "_blank" $ toHtml $ savedItemTitle comment
        if link == permalink then "" else
          a ! class_ "inline-block ml-2 text-gray-400 hover:text-gray-600" ! (href . fromString $ permalink) ! -- d-inline-block
            target "_blank"
            $ i "" ! class_ "far fa-comment" -- ml-1 text-muted removed from here
        H.span ! class_ "inline-block px-2 py-0.5 text-xs font-semibold rounded bg-blue-200 text-blue-800 ml-2" $ -- badge badge-info ml-1
          toHtml $ savedItemScore comment
      small ! class_ "text-sm text-gray-500" $ toHtml $ formatDatetime $ savedItemCreatedUtc comment -- text-muted equivalent
    H.div ! class_ "text-sm text-gray-600 mt-1" $ do -- small wrapper for metadata
      let author = savedItemAuthor comment
      let subreddit = savedItemSubreddit comment
      itemMetadata author subreddit
    let maybeBody = savedItemBody comment
    case maybeBody of
      Nothing -> ""
      Just body ->
        H.div ! class_ "prose prose-sm mt-2" $ preEscapedToHtml body -- Added prose for rich text styling
    H.div ! class_ "text-sm text-gray-600 mt-1" $ do -- small wrapper for parent author
      let maybeCommentAuthor = savedItemParentAuthor comment
      case maybeCommentAuthor of
        Nothing -> ""
        Just author -> do
          "comment by "
          a ! class_ "inline-block hover:underline text-blue-600 mx-1" !
            (href . fromString) ("https://reddit.com/u/" ++ author) !
            target "_blank" $
            string author
