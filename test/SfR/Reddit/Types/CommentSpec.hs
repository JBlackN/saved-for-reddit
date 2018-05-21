{-# LANGUAGE OverloadedStrings #-}
module SfR.Reddit.Types.CommentSpec where

import Data.Aeson
import Data.ByteString.Lazy as BSL
import Test.Hspec

import SfR.Reddit.Types.Comment as TC

test_listing_json :: ByteString
test_listing_json = "{ \"data\" : { \"after\": null, \"children\": [] } }"

test_saved_comment_json :: ByteString
test_saved_comment_json = BSL.concat [
    "{ \"kind\": \"comment\", \"data\": { \"name\": \"name\", ",
    "\"author\": \"author\", \"subreddit\": \"sub\", ",
    "\"body_html\": \"body\", \"score\": 0, ",
    "\"permalink\": \"permalink\", \"created_utc\": 0, ",
    "\"link_author\": \"author\", \"link_title\": \"title\", ",
    "\"link_permalink\": \"permalink\"} }"
  ]

test_listing :: CommentListing
test_listing = CommentListing { TC.data' = test_listing_data }

test_listing_data :: CommentListingData
test_listing_data = CommentListingData { TC.after = Nothing
                                       , TC.children = []
                                       }

test_saved_comment :: SavedComment
test_saved_comment = SavedComment { TC.kind'' = "comment"
                                  , TC.data'' = test_saved_comment_data
                                  }

test_saved_comment_data :: SavedCommentData
test_saved_comment_data = SavedCommentData { TC.name = "name"
                                           , TC.author = "author"
                                           , TC.subreddit = "sub"
                                           , TC.body_html = "body"
                                           , TC.score = 0
                                           , TC.permalink = "permalink"
                                           , TC.created_utc = 0
                                           , TC.link_author = "author"
                                           , TC.link_title = "title"
                                           , TC.link_permalink = "permalink"
                                           }

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "CommentListing" $
    it "decoding from JSON works properly" $
      (decode test_listing_json :: Maybe CommentListing) `shouldBe` Just test_listing
  describe "SavedComment" $
    it "decoding from JSON works properly" $
      (decode test_saved_comment_json :: Maybe SavedComment) `shouldBe` Just test_saved_comment
