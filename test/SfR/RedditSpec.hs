module SfR.RedditSpec where

import Data.Char (isSpace)
import Test.Hspec

import SfR.Metadata (userAgent)
import SfR.Reddit

removeWhitespace :: String -> String
removeWhitespace = concatMap (\c -> if isSpace c then [] else [c])

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "savedRequest" $
    it "constructs Reddit API request for saved items properly" $ do
      request <- savedRequest "type" "token" "user" "after"
      (unlines . map removeWhitespace . lines . show) request
         `shouldBe` "Request{\nhost=\"oauth.reddit.com\"\nport=443\nsecure=" ++
                    "True\nrequestHeaders=[(\"Authorization\",\"<REDACTED>\")" ++
                    ",(\"User-Agent\",\"" ++ removeWhitespace userAgent ++
                    "\")]\npath=\"/user/user/saved\"\nqueryString=\"?type=" ++
                    "type&sort=new&t=all&limit=100&raw_json=1&after=after" ++
                    "\"\nmethod=\"GET\"\nproxy=Nothing\nrawBody=False\n" ++
                    "redirectCount=10\nresponseTimeout=ResponseTimeout" ++
                    "Default\nrequestVersion=HTTP/1.1\n}\n"
