{-# LANGUAGE OverloadedStrings #-}
module SfR.Reddit where

import Control.Monad
import Data.ByteString as BS hiding (map)
import Data.ByteString.Char8 as BSC hiding (map)
import Network.HTTP.Simple

import SfR.Metadata (user_agent)
import SfR.Reddit.Types (RedditUser)
import SfR.Reddit.Types.Comment as RC ( CommentListing, SavedCommentData
                                      , after, children, data', data''
                                      )
import SfR.Reddit.Types.Post as RP ( PostListing, SavedPostData
                                   , after, children, data', data''
                                   )

identity :: String -> IO RedditUser
identity access_token = do
  let auth_header = BS.concat ["Bearer ", BSC.pack access_token]
  let user_agent_header = BSC.pack user_agent

  request''    <- parseRequest "https://oauth.reddit.com/api/v1/me"
  let request' =  addRequestHeader "User-Agent" user_agent_header request''
  let request  =  addRequestHeader "Authorization" auth_header request'

  liftM (\response -> getResponseBody response :: RedditUser) (httpJSON request)

savedPosts :: String -> String -> String -> IO [SavedPostData]
savedPosts access_token username after = do
  let auth_header = BS.concat ["Bearer ", BSC.pack access_token]
  let user_agent_header = BSC.pack user_agent
  let path = "/user/" ++ username ++ "/saved"
  let query = "?type=links&sort=new&t=all&limit=100&after=" ++ after

  request''    <- parseRequest ("https://oauth.reddit.com" ++ path ++ query)
  let request' =  addRequestHeader "User-Agent" user_agent_header request''
  let request  =  addRequestHeader "Authorization" auth_header request'

  listing <- liftM (\response -> getResponseBody response :: PostListing) (httpJSON request)
  let posts = (map (\post -> RP.data'' post) . RP.children . RP.data') listing
  let after = (RP.after . RP.data') listing

  case after of
    Just value -> do
      next_posts <- savedPosts access_token username value
      return (posts ++ next_posts)
    Nothing -> do
      return posts

savedComments :: String -> String -> String -> IO [SavedCommentData]
savedComments access_token username after = do
  let auth_header = BS.concat ["Bearer ", BSC.pack access_token]
  let user_agent_header = BSC.pack user_agent
  let path = "/user/" ++ username ++ "/saved"
  let query = "?type=comments&sort=new&t=all&limit=100&after=" ++ after

  request''    <- parseRequest ("https://oauth.reddit.com" ++ path ++ query)
  let request' =  addRequestHeader "User-Agent" user_agent_header request''
  let request  =  addRequestHeader "Authorization" auth_header request'

  listing <- liftM (\response -> getResponseBody response :: CommentListing) (httpJSON request)
  let comments = (map (\comment -> RC.data'' comment) . RC.children . RC.data') listing
  let after = (RC.after . RC.data') listing

  case after of
    Just value -> do
      next_comments <- savedComments access_token username value
      return (comments ++ next_comments)
    Nothing -> do
      return comments
