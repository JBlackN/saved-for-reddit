{-# LANGUAGE OverloadedStrings #-}
module SfR.Templates.HelpersSpec where

import Database.Persist.Sql (toSqlKey)
import Test.Hspec
import Text.Blaze.Html.Renderer.String (renderHtml)

import SfR.Storage
import SfR.Templates.Helpers

saved_post_item :: SavedItem
saved_post_item =
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
            }

saved_comment_item :: SavedItem
saved_comment_item =
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
            }

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "format_datetime" $ do
    it "converts UNIX timestamp to formatted date and time" $ do
      format_datetime 1526850351 `shouldBe` "20.05.2018 21:05:51"
  describe "subreddit_filter_option" $ do
    context "when option matches selected subreddit" $ do
      it "generates HTML select filter option" $ do
        let option = subreddit_filter_option "AskReddit" "AskReddit"
        renderHtml option `shouldBe` "<option value=\"AskReddit\" " ++
                                     "selected=\"selected\">AskReddit</option>"
    context "otherwise" $ do
      it "generates HTML select filter option" $ do
        let option = subreddit_filter_option "AskReddit" "AskPhotography"
        renderHtml option `shouldBe` "<option value=\"AskPhotography\">" ++
                                     "AskPhotography</option>"
  describe "show_saved_item" $ do
    context "when item is a post" $ do
      it "renders it as HTML list item" $ do
        let item = show_saved_item saved_post_item
        renderHtml item `shouldBe` "<li class=\"list-group-item flex-column " ++
                                   "align-items-start\"><div class=\"media\">" ++
                                   "<img class=\"mr-3\" " ++
                                   "src=\"https://example.org/test.jpg\">" ++
                                   "<div class=\"media-body\"><div " ++
                                   "class=\"d-flex w-100 " ++
                                   "justify-content-between\"><h2 " ++
                                   "class=\"mb-1 saved-item-title\">" ++
                                   "<a class=\"d-inline-block\" " ++
                                   "href=\"https://www.reddit.com/r/AskReddit/" ++
                                   "...\" target=\"_blank\">Sample post 1</a>" ++
                                   "<span class=\"badge badge-info ml-1\">" ++
                                   "9001</span></h2><small>20.05.2018 " ++
                                   "21:05:51</small></div><small>by<a " ++
                                   "class=\"d-inline-block\" " ++
                                   "href=\"https://reddit.com/u/JBlackN\" " ++
                                   "target=\"_blank\">JBlackN</a>in<a " ++
                                   "class=\"d-inline-block\" href=\"https://" ++
                                   "reddit.com/r/AskReddit\" target=\"_blank\">" ++
                                   "r/AskReddit</a></small><p>Lorem ipsum " ++
                                   "dolor sit amet.</p></div></div></li>"
    context "when item is a comment" $ do
      it "renders it as HTML list item" $ do
        let item = show_saved_item saved_comment_item
        renderHtml item `shouldBe` "<li class=\"list-group-item flex-column " ++
                                   "align-items-start\"><div class=\"d-flex " ++
                                   "w-100 justify-content-between\"><h2 " ++
                                   "class=\"mb-1 saved-item-title\"><a " ++
                                   "class=\"d-inline-block\" " ++
                                   "href=\"https://www.reddit.com/r/AskReddit/" ++
                                   "...\" target=\"_blank\">Sample post 1" ++
                                   "</a><span class=\"badge badge-info " ++
                                   "ml-1\">9001</span></h2><small>20.05.2018 " ++
                                   "21:05:51</small></div><small>by<a " ++
                                   "class=\"d-inline-block\" " ++
                                   "href=\"https://reddit.com/u/JBlackN\" " ++
                                   "target=\"_blank\">JBlackN</a>in<a " ++
                                   "class=\"d-inline-block\" " ++
                                   "href=\"https://reddit.com/r/AskReddit\" " ++
                                   "target=\"_blank\">r/AskReddit</a></small>" ++
                                   "<p>Lorem ipsum dolor sit amet.</p><small>" ++
                                   "comment by<a class=\"d-inline-block\" " ++
                                   "href=\"https://reddit.com/u/JBlackN\" " ++
                                   "target=\"_blank\">JBlackN</a></small></li>"
