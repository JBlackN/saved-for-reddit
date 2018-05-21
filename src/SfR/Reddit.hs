{-|
Module     : SfR.Reddit
Description: Reddit API communication methods
Copyright  : (c) Petr Schmied, 2018
License    : MIT
Maintainer : peter9209@gmail.com
Stability  : stable
Portability: portable

Module defines methods for communicating with [Reddit](https://www.reddit.com)
API. See <https://www.reddit.com/dev/api/>.
-}
{-# LANGUAGE OverloadedStrings #-}
module SfR.Reddit where

import Control.Monad
import Data.ByteString as BS hiding (map)
import Data.ByteString.Char8 as BSC hiding (map)
import Network.HTTP.Simple

import SfR.Metadata (userAgent)
import SfR.Reddit.Types (RedditUser)
import SfR.Reddit.Types.Comment as RC ( CommentListing, SavedCommentData
                                      , after, children, data', data''
                                      )
import SfR.Reddit.Types.Post as RP ( PostListing, SavedPostData
                                   , after, children, data', data''
                                   )

-- | Gets access token's owner identity.
--
-- Uses [Reddit](https://www.reddit.com) API access token to obtain its
-- owner's [Reddit](https://www.reddit.com) username.
identity :: String -- ^ [Reddit](https://www.reddit.com) API access token.
         -> IO RedditUser -- ^ [Reddit](https://www.reddit.com) user's username.
identity access_token = do
  let auth_header = BS.concat ["Bearer ", BSC.pack access_token]
  let user_agent_header = BSC.pack userAgent

  request''    <- parseRequest "https://oauth.reddit.com/api/v1/me"
  let request' =  addRequestHeader "User-Agent" user_agent_header request''
  let request  =  addRequestHeader "Authorization" auth_header request'

  (\response -> getResponseBody response :: RedditUser) <$> httpJSON request

-- | Prepares [Reddit](https://www.reddit.com) API request for obtaining user's
--   saved items.
--
--   See <https://www.reddit.com/dev/api/#GET_user_{username}_saved>.
savedRequest :: String -- ^ Saved items' type (@links@ or @comments@). See
                       --   <https://www.reddit.com/dev/api/#GET_user_{username}_saved>.
             -> String -- ^ [Reddit](https://www.reddit.com) API access token.
             -> String -- ^ [Reddit](https://www.reddit.com) username of user
                       --   whose saved items to get.
             -> String -- ^ [Reddit](https://www.reddit.com) saved item
                       --   identifier to start getting items from
                       --   (excluding itself). See
                       --   <https://www.reddit.com/dev/api/#listings>.
             -> IO Request -- ^ [Reddit](https://www.reddit.com) API request.
savedRequest type' access_token username after = do
  let auth_header = BS.concat ["Bearer ", BSC.pack access_token]
  let user_agent_header = BSC.pack userAgent
  let path = "/user/" ++ username ++ "/saved"
  let query = "?type=" ++ type' ++
              "&sort=new&t=all&limit=100&raw_json=1&after=" ++ after

  request''    <- parseRequest ("https://oauth.reddit.com" ++ path ++ query)
  let request' =  addRequestHeader "User-Agent" user_agent_header request''
  let request  =  addRequestHeader "Authorization" auth_header request'
  return request

-- | Gets [Reddit](https://www.reddit.com) user's saved posts.
--
-- Recurses to obtain all available posts using @after@ parameter (see
-- <https://www.reddit.com/dev/api/#listings>).
--
-- @See also:@ 'savedRequest', "SfR.Reddit.Types.Post".
savedPosts :: String -- ^ [Reddit](https://www.reddit.com) API access token.
           -> String -- ^ [Reddit](https://www.reddit.com) username of user
                     --   whose saved posts to get.
           -> String -- ^ [Reddit](https://www.reddit.com) saved post
                     --   identifier to start getting posts from
                     --   (excluding itself). See
                     --   <https://www.reddit.com/dev/api/#listings>.
           -> IO [SavedPostData] -- ^ Saved [Reddit](https://www.reddit.com)
                                 --   posts.
savedPosts access_token username after = do
  request <- savedRequest "links" access_token username after
  listing <-
    (\response -> getResponseBody response :: PostListing) <$> httpJSON request
  let posts = (map RP.data'' . RP.children . RP.data') listing
  let after = (RP.after . RP.data') listing

  case after of
    Just value -> do
      next_posts <- savedPosts access_token username value
      return (posts ++ next_posts)
    Nothing ->
      return posts

-- | Gets [Reddit](https://www.reddit.com) user's saved comments.
--
-- Recurses to obtain all available comments using @after@ parameter (see
-- <https://www.reddit.com/dev/api/#listings>).
--
-- @See also:@ 'savedRequest', "SfR.Reddit.Types.Comment".
savedComments :: String -- ^ [Reddit](https://www.reddit.com) API access token.
              -> String -- ^ [Reddit](https://www.reddit.com) username of user
                        --   whose saved comments to get.
              -> String -- ^ [Reddit](https://www.reddit.com) saved comment
                        --   identifier to start getting comments from
                        --   (excluding itself). See
                        --   <https://www.reddit.com/dev/api/#listings>.
              -> IO [SavedCommentData] -- ^ Saved [Reddit](https://www.reddit.com)
                                       --   comments.
savedComments access_token username after = do
  request <- savedRequest "comments" access_token username after
  listing <-
    (\response -> getResponseBody response :: CommentListing) <$> httpJSON request
  let comments = (map RC.data'' . RC.children . RC.data') listing
  let after = (RC.after . RC.data') listing

  case after of
    Just value -> do
      next_comments <- savedComments access_token username value
      return (comments ++ next_comments)
    Nothing ->
      return comments
