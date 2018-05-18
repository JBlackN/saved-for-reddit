{-# LANGUAGE OverloadedStrings #-}
module SfR.Reddit where

import Control.Monad
import Data.ByteString as BS
import Data.ByteString.Char8 as BSC
import Network.HTTP.Simple

import SfR.Metadata (user_agent)
import SfR.Reddit.Types (RedditUser)

identity :: String -> IO RedditUser
identity access_token = do
  let auth_header = BS.concat ["Bearer ", BSC.pack access_token]
  let user_agent_header = BSC.pack user_agent

  request''    <- parseRequest "https://oauth.reddit.com/api/v1/me"
  let request' =  addRequestHeader "User-Agent" user_agent_header request''
  let request  =  addRequestHeader "Authorization" auth_header request'

  liftM (\response -> getResponseBody response :: RedditUser) (httpJSON request)
