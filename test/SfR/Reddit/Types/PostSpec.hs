{-# LANGUAGE OverloadedStrings #-}
module SfR.Reddit.Types.PostSpec where

import Data.Aeson
import Data.ByteString.Lazy as BSL
import Test.Hspec

import SfR.Reddit.Types.Post as TP

test_listing_json :: ByteString
test_listing_json = "{ \"data\" : { \"after\": null, \"children\": [] } }"

test_saved_post_json :: ByteString
test_saved_post_json = BSL.concat [
    "{ \"kind\": \"link\", \"data\": { \"name\": \"name\", ",
    "\"author\": \"author\", \"subreddit\": \"sub\", ",
    "\"selftext_html\": null, \"score\": 0, ",
    "\"thumbnail\": \"self\", \"title\": \"title\", ",
    "\"url\": \"url\", \"permalink\": \"permalink\", ",
    "\"created_utc\": 0} }"
  ]

test_listing :: PostListing
test_listing = PostListing { TP.data' = test_listing_data }

test_listing_data :: PostListingData
test_listing_data = PostListingData { TP.after = Nothing
                                    , TP.children = []
                                    }

test_saved_post :: SavedPost
test_saved_post = SavedPost { TP.kind'' = "link"
                            , TP.data'' = test_saved_post_data
                            }

test_saved_post_data :: SavedPostData
test_saved_post_data = SavedPostData { TP.name = "name"
                                     , TP.author = "author"
                                     , TP.subreddit = "sub"
                                     , TP.selftext_html = Nothing
                                     , TP.score = 0
                                     , TP.thumbnail = "self"
                                     , TP.title = "title"
                                     , TP.url = "url"
                                     , TP.permalink = "permalink"
                                     , TP.created_utc = 0
                                     }

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "PostListing" $ do
    it "decoding from JSON works properly" $ do
      (decode test_listing_json :: Maybe PostListing) `shouldBe` Just test_listing
  describe "SavedPost" $ do
    it "decoding from JSON works properly" $ do
      (decode test_saved_post_json :: Maybe SavedPost) `shouldBe` Just test_saved_post
