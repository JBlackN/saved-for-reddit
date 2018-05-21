{-|
Module     : SfR.Reddit.Types.Post
Description: Reddit post data types
Copyright  : (c) Petr Schmied, 2018
License    : MIT
Maintainer : peter9209@gmail.com
Stability  : stable
Portability: portable

Module defines data types for [Reddit](https://www.reddit.com) posts.
-}
{-# LANGUAGE DeriveGeneric #-}
module SfR.Reddit.Types.Post where

import Data.Aeson
import Data.Int (Int64)
import Data.Text hiding (take)
import GHC.Generics

-- | [Reddit](https://www.reddit.com) post listing.
newtype PostListing = PostListing { -- | Post listing contents.
                                    data' :: PostListingData
                                  } deriving (Generic, Show, Eq)

-- | [Reddit](https://www.reddit.com) post listing contents.
data PostListingData = PostListingData { -- | Identifier of the last
                                         --   post contained within
                                         --   this listing. See
                                         --   <https://www.reddit.com/dev/api#fullnames>.
                                         after :: Maybe String
                                         -- | [Reddit](https://www.reddit.com)
                                         --   posts.
                                       , children :: [SavedPost]
                                       } deriving (Generic, Show, Eq)

-- | [Reddit](https://www.reddit.com) post.
data SavedPost = SavedPost { -- | @"t3"@, see
                             --   <https://www.reddit.com/dev/api#fullnames>.
                             kind'' :: String
                             -- | [Reddit](https://www.reddit.com)
                             --   post data.
                           , data'' :: SavedPostData
                           } deriving (Generic, Show, Eq)

-- | [Reddit](https://www.reddit.com) post data.
data SavedPostData = SavedPostData { -- | Post identifier, see
                                     --   <https://www.reddit.com/dev/api#fullnames>.
                                     name :: String
                                     -- | Post's author.
                                   , author :: String
                                     -- | Post's subreddit.
                                   , subreddit :: String
                                     -- | Post's body in raw HTML.
                                   , selftext_html :: Maybe Text
                                     -- | Post's score (upvotes
                                     --   &#x2212; downvotes).
                                   , score :: Int
                                     -- | Post's thumbnail URL or identifier
                                     --   (see 'SfR.Storage.normalizeSaved').
                                   , thumbnail :: String
                                     -- | Post's title.
                                   , title :: String
                                     -- | Post's external link if applicable.
                                     --   Internal link otherwise.
                                   , url :: String
                                     -- | Post's internal link.
                                   , permalink :: String
                                     -- | Post's creation date and
                                     --   time (UNIX timestamp).
                                   , created_utc :: Int64
                                   } deriving (Generic, Show, Eq)

-- | Extract [Reddit](https://www.reddit.com) post listing from JSON.
instance FromJSON PostListing where
  parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = take 4
    }
-- | Extract [Reddit](https://www.reddit.com) post listing data from JSON.
instance FromJSON PostListingData
-- | Extract [Reddit](https://www.reddit.com) post from JSON.
instance FromJSON SavedPost where
  parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = take 4
    }
-- | Extract [Reddit](https://www.reddit.com) post data from JSON.
instance FromJSON SavedPostData
