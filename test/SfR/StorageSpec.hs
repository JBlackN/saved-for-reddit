{-# LANGUAGE OverloadedStrings #-}
module SfR.StorageSpec where

import Database.Persist.Sql (toSqlKey)
import Test.Hspec

import SfR.Reddit.Types.Comment as TC
import SfR.Reddit.Types.Post as TP
import SfR.Storage

test_posts :: [SavedPostData]
test_posts = [
    SavedPostData { TP.name = "t3_xxxxxx"
                  , TP.author = "JBlackN"
                  , TP.subreddit = "AskReddit"
                  , TP.selftext_html = Just "Lorem ipsum dolor sit amet."
                  , TP.score = 9001
                  , TP.thumbnail = "https://example.org/test.jpg"
                  , TP.title = "Sample post 1"
                  , TP.url = "https://www.reddit.com/r/AskReddit/..."
                  , TP.permalink = "/r/AskReddit/..."
                  , TP.created_utc = 1526850351
                  },
    SavedPostData { TP.name = "t3_yyyyyy"
                  , TP.author = "JBlackN"
                  , TP.subreddit = "AskReddit"
                  , TP.selftext_html = Nothing
                  , TP.score = 9001
                  , TP.thumbnail = "nsfw"
                  , TP.title = "Sample post 2"
                  , TP.url = "https://www.reddit.com/r/AskReddit/..."
                  , TP.permalink = "/r/AskReddit/..."
                  , TP.created_utc = 1526850351
                  }
  ]

test_comments :: [SavedCommentData]
test_comments = [
    SavedCommentData { TC.name = "t1_xxxxxx"
                     , TC.author = "JBlackN"
                     , TC.subreddit = "AskReddit"
                     , TC.body_html = "Lorem ipsum dolor sit amet."
                     , TC.score = 9001
                     , TC.permalink = "/r/AskReddit/..."
                     , TC.created_utc = 1526850351
                     , TC.link_author = "JBlackN"
                     , TC.link_title = "Sample post 1"
                     , TC.link_permalink = "https://www.reddit.com/r/AskReddit/..."
                     },
    SavedCommentData { TC.name = "t1_yyyyyy"
                     , TC.author = "JBlackN"
                     , TC.subreddit = "AskReddit"
                     , TC.body_html = "Lorem ipsum dolor sit amet."
                     , TC.score = 9001
                     , TC.permalink = "/r/AskReddit/..."
                     , TC.created_utc = 1526850351
                     , TC.link_author = "JBlackN"
                     , TC.link_title = "Sample post 2"
                     , TC.link_permalink = "https://www.reddit.com/r/AskReddit/..."
                     }
  ]

test_items :: [SavedItem]
test_items = [
    SavedItem { savedItemIdentifier = "t3_xxxxxx"
              , savedItemAuthor = "JBlackN"
              , savedItemParentAuthor = Nothing
              , savedItemThumbnail = Just "https://example.org/test.jpg"
              , savedItemTitle = "Sample post 1"
              , savedItemLink = "https://www.reddit.com/r/AskReddit/..."
              , savedItemPermalink = "https://www.reddit.com/r/AskReddit/..."
              , savedItemBody = Just "Lorem ipsum dolor sit amet."
              , savedItemSubreddit = "AskReddit"
              , savedItemScore = 9001
              , savedItemCreatedUtc = 1526850351
              , savedItemUserId = toSqlKey 1
              },
    SavedItem { savedItemIdentifier = "t3_yyyyyy"
              , savedItemAuthor = "JBlackN"
              , savedItemParentAuthor = Nothing
              , savedItemThumbnail = Nothing
              , savedItemTitle = "Sample post 2"
              , savedItemLink = "https://www.reddit.com/r/AskReddit/..."
              , savedItemPermalink = "https://www.reddit.com/r/AskReddit/..."
              , savedItemBody = Nothing
              , savedItemSubreddit = "AskReddit"
              , savedItemScore = 9001
              , savedItemCreatedUtc = 1526850351
              , savedItemUserId = toSqlKey 1
              },
    SavedItem { savedItemIdentifier = "t1_xxxxxx"
              , savedItemAuthor = "JBlackN"
              , savedItemParentAuthor = Just "JBlackN"
              , savedItemThumbnail = Nothing
              , savedItemTitle = "Sample post 1"
              , savedItemLink = "https://www.reddit.com/r/AskReddit/..."
              , savedItemPermalink = "https://www.reddit.com/r/AskReddit/..."
              , savedItemBody = Just "Lorem ipsum dolor sit amet."
              , savedItemSubreddit = "AskReddit"
              , savedItemScore = 9001
              , savedItemCreatedUtc = 1526850351
              , savedItemUserId = toSqlKey 1
              },
    SavedItem { savedItemIdentifier = "t1_yyyyyy"
              , savedItemAuthor = "JBlackN"
              , savedItemParentAuthor = Just "JBlackN"
              , savedItemThumbnail = Nothing
              , savedItemTitle = "Sample post 2"
              , savedItemLink = "https://www.reddit.com/r/AskReddit/..."
              , savedItemPermalink = "https://www.reddit.com/r/AskReddit/..."
              , savedItemBody = Just "Lorem ipsum dolor sit amet."
              , savedItemSubreddit = "AskReddit"
              , savedItemScore = 9001
              , savedItemCreatedUtc = 1526850351
              , savedItemUserId = toSqlKey 1
              }
  ]

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "normalizeSaved" $
    context "while assigning to user by her/his ID" $
      it "merges saved posts and comments into single array of items" $
        normalizeSaved 1 test_posts test_comments `shouldBe` test_items
