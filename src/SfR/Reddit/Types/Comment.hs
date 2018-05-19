{-# LANGUAGE DeriveGeneric #-}
module SfR.Reddit.Types.Comment where

import Data.Aeson
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
                                         , created_utc :: Double
                                         , link_author :: String
                                         , link_title :: String
                                         , link_url :: String
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
