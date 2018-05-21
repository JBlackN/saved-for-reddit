{-# LANGUAGE DeriveGeneric #-}
module SfR.Reddit.Types.Comment where

import Data.Aeson
import Data.Int (Int64)
import Data.Text hiding (take)
import GHC.Generics

newtype CommentListing = CommentListing { data' :: CommentListingData
                                        } deriving (Generic, Show, Eq)

data CommentListingData = CommentListingData { after :: Maybe String
                                             , children :: [SavedComment]
                                             } deriving (Generic, Show, Eq)

data SavedComment = SavedComment { kind'' :: String
                                 , data'' :: SavedCommentData
                                 } deriving (Generic, Show, Eq)

data SavedCommentData = SavedCommentData { name :: String
                                         , author :: String
                                         , subreddit :: String
                                         , body_html :: Text
                                         , score :: Int
                                         , permalink :: String
                                         , created_utc :: Int64
                                         , link_author :: String
                                         , link_title :: String
                                         , link_permalink :: String
                                         } deriving (Generic, Show, Eq)

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
