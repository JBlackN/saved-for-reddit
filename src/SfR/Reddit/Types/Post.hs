{-# LANGUAGE DeriveGeneric #-}
module SfR.Reddit.Types.Post where

import Data.Aeson
import Data.Int (Int64)
import GHC.Generics

data PostListing = PostListing { data' :: PostListingData
                               } deriving (Generic, Show)

data PostListingData = PostListingData { after :: Maybe String
                                       , children :: [SavedPost]
                                       } deriving (Generic, Show)

data SavedPost = SavedPost { kind'' :: String
                           , data'' :: SavedPostData
                           } deriving (Generic, Show)

data SavedPostData = SavedPostData { name :: String
                                   , author :: String
                                   , subreddit :: String
                                   , selftext_html :: Maybe String
                                   , score :: Int
                                   , thumbnail :: String
                                   , title :: String
                                   , url :: String
                                   , permalink :: String
                                   , created_utc :: Int64
                                   } deriving (Generic, Show)

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
