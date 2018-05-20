{-# LANGUAGE DeriveGeneric #-}
module SfR.Reddit.Types.Comment where

import Data.Aeson
import Data.Int (Int64)
import GHC.Generics

data CommentListing = CommentListing { data' :: CommentListingData
                                     } deriving (Generic, Show)

data CommentListingData = CommentListingData { after :: Maybe String
                                             , children :: [SavedComment]
                                             } deriving (Generic, Show)

data SavedComment = SavedComment { kind'' :: String
                                 , data'' :: SavedCommentData
                                 } deriving (Generic, Show)

data SavedCommentData = SavedCommentData { name :: String
                                         , author :: String
                                         , subreddit :: String
                                         , body_html :: String
                                         , score :: Int
                                         , permalink :: String
                                         , created_utc :: Int64
                                         , link_author :: String
                                         , link_title :: String
                                         , link_permalink :: String
                                         } deriving (Generic, Show)

instance FromJSON CommentListing where
  parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = take 4
    }
instance FromJSON CommentListingData
instance FromJSON SavedComment where
  parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = take 4
    }
instance FromJSON SavedCommentData
