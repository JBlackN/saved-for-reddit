module SfR.MetadataSpec where

import Data.Version (showVersion)
import qualified Paths_saved_for_reddit as App (version)
import Test.Hspec

import SfR.Metadata

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "version" $
    it "gets application's correct version" $
      version `shouldBe` showVersion App.version
  describe "userAgent" $
    it "generates correct User-Agent for the application" $ do
      let ua = "haskell:saved-for-reddit:v" ++ version ++ " (by /u/JBlackN)"
      userAgent `shouldBe` ua
