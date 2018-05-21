{-|
Module     : SfR.Reddit.Types.Comment
Description: Reddit comment data types
Copyright  : (c) Petr Schmied, 2018
License    : MIT
Maintainer : peter9209@gmail.com
Stability  : stable
Portability: portable

Module defines data types for [Reddit](https://www.reddit.com) comments.
-}
{-# LANGUAGE DeriveGeneric #-}
module SfR.Reddit.Types.Comment where

import Data.Aeson
import Data.Int (Int64)
import Data.Text hiding (take)
import GHC.Generics

-- | [Reddit](https://www.reddit.com) comment listing.
newtype CommentListing = CommentListing { -- | Comment listing contents.
                                          data' :: CommentListingData
                                        } deriving (Generic, Show, Eq)

-- | [Reddit](https://www.reddit.com) comment listing contents.
data CommentListingData = CommentListingData { -- | Identifier of the last
                                               --   comment contained within
                                               --   this listing. See
                                               --   <https://www.reddit.com/dev/api#fullnames>.
                                               after :: Maybe String
                                               -- | [Reddit](https://www.reddit.com)
                                               --   comments.
                                             , children :: [SavedComment]
                                             } deriving (Generic, Show, Eq)

-- | [Reddit](https://www.reddit.com) comment.
data SavedComment = SavedComment { -- | @"t1"@, see
                                   --   <https://www.reddit.com/dev/api#fullnames>.
                                   kind'' :: String
                                   -- | [Reddit](https://www.reddit.com)
                                   --   comment data.
                                 , data'' :: SavedCommentData
                                 } deriving (Generic, Show, Eq)

-- | [Reddit](https://www.reddit.com) comment data.
data SavedCommentData = SavedCommentData { -- | Comment's identifier, see
                                           --   <https://www.reddit.com/dev/api#fullnames>.
                                           name :: String
                                           -- | Comment's author.
                                         , author :: String
                                           -- | Parent post's subreddit.
                                         , subreddit :: String
                                           -- | Comment's body in raw HTML.
                                         , body_html :: Text
                                           -- | Comment's score (upvotes
                                           --   &#x2212; downvotes).
                                         , score :: Int
                                           -- | Link to the comment.
                                         , permalink :: String
                                           -- | Comment's creation date and
                                           --   time (UNIX timestamp).
                                         , created_utc :: Int64
                                           -- | Parent post's author.
                                         , link_author :: String
                                           -- | Parent post's title.
                                         , link_title :: String
                                           -- | Link to the parent post.
                                         , link_permalink :: String
                                         } deriving (Generic, Show, Eq)

-- | Extract [Reddit](https://www.reddit.com) comment listing from JSON.
instance FromJSON CommentListing where
  parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = take 4
    }
-- | Extract [Reddit](https://www.reddit.com) comment listing data from JSON.
instance FromJSON CommentListingData
-- | Extract [Reddit](https://www.reddit.com) comment from JSON.
instance FromJSON SavedComment where
  parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = take 4
    }
-- | Extract [Reddit](https://www.reddit.com) comment data from JSON.
instance FromJSON SavedCommentData
