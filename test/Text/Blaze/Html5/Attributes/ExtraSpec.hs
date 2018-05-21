{-# LANGUAGE OverloadedStrings #-}
module Text.Blaze.Html5.Attributes.ExtraSpec where

import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Test.Hspec

import Text.Blaze.Html5.Attributes.Extra as AE

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "crossorigin" $
    it "renders as HTML attribute correctly" $
      renderHtml (H.link ! AE.crossorigin "test")
        `shouldBe` "<link crossorigin=\"test\">"
  describe "integrity" $
    it "renders as HTML attribute correctly" $
      renderHtml (H.link ! AE.integrity "test")
        `shouldBe` "<link integrity=\"test\">"
  describe "role" $
    it "renders as HTML attribute correctly" $
      renderHtml (H.link ! AE.role "test")
        `shouldBe` "<link role=\"test\">"
  describe "data_" $
    it "renders as HTML attribute correctly" $
      renderHtml (H.div "" ! AE.data_ "key" "value")
        `shouldBe` "<div data-key=\"value\"></div>"
