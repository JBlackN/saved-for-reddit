{-# LANGUAGE DeriveGeneric #-}
module SfR.Reddit.Types.Post where

import Data.Aeson
import Data.Int (Int64)
import Data.Text hiding (take)
import GHC.Generics

newtype PostListing = PostListing { data' :: PostListingData
                                  } deriving (Generic, Show, Eq)

data PostListingData = PostListingData { after :: Maybe String
                                       , children :: [SavedPost]
                                       } deriving (Generic, Show, Eq)

data SavedPost = SavedPost { kind'' :: String
                           , data'' :: SavedPostData
                           } deriving (Generic, Show, Eq)

data SavedPostData = SavedPostData { name :: String
                                   , author :: String
                                   , subreddit :: String
                                   , selftext_html :: Maybe Text
                                   , score :: Int
                                   , thumbnail :: String
                                   , title :: String
                                   , url :: String
                                   , permalink :: String
                                   , created_utc :: Int64
                                   } deriving (Generic, Show, Eq)

instance FromJSON PostListing where
  parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = take 4
    }
instance FromJSON PostListingData
instance FromJSON SavedPost where
  parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = take 4
    }
instance FromJSON SavedPostData
