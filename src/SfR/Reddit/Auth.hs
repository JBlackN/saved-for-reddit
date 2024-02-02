{-|
Module     : SfR.Reddit.Auth
Description: Reddit API authentication
Copyright  : (c) Petr Schmied, 2018
License    : MIT
Maintainer : peter9209@gmail.com
Stability  : stable
Portability: portable

Module defines methods for authenticating with [Reddit](https://www.reddit.com)
API. See <https://github.com/reddit-archive/reddit/wiki/OAuth2>.
-}
{-# LANGUAGE OverloadedStrings #-}
module SfR.Reddit.Auth where

import Control.Monad (liftM)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Network.HTTP.Conduit
import Network.HTTP.Simple

import SfR.Config (callback_url, client_id, client_secret, sfrConfig)
import SfR.Metadata (userAgent)
import SfR.Reddit.Types (AccessToken)

-- | Gets [Reddit](https://www.reddit.com) API access token.
--
-- Uses API authorization code obtained beforehand (see 'SfR.Actions.callback').
--
-- (1) Gets @client_id@, @client_secret@ and @callback_url@ from application's
--     configuration (see "SfR.Config").
-- (2) Prepares [Reddit](https://www.reddit.com) API request. See
--     <https://github.com/reddit-archive/reddit/wiki/OAuth2>.
-- (3) Sends request and parses access token from the response (see
--     'SfR.Reddit.Types.AccessToken').
getAccessToken :: String -- ^ [Reddit](https://www.reddit.com) API
                         --   authorization code.
               -> IO AccessToken -- ^ [Reddit](https://www.reddit.com) API
                                 --   access token with metadata.
getAccessToken auth_token = do
  client_id <- client_id <$> sfrConfig
  client_secret <- client_secret <$> sfrConfig
  callback_url <- callback_url <$> sfrConfig

  let auth_header =
        BS.concat [
          "Basic ", B64.encode $ BSC.pack (client_id ++ ":" ++ client_secret)]
  let content_header = "application/x-www-form-urlencoded"
  let user_agent_header = BSC.pack userAgent
  let request_body =
        LBSC.pack (
          "grant_type=authorization_code&code=" ++ auth_token ++
          "&redirect_uri=" ++ callback_url)

  request'''''    <- parseRequest "https://www.reddit.com/api/v1/access_token"
  let request'''' =  request''''' { method = "POST" }
  let request'''  =  addRequestHeader "Authorization" auth_header request''''
  let request''   =  addRequestHeader "Content-Type" content_header request'''
  let request' =  addRequestHeader "User-Agent" user_agent_header request''
  let request    =  setRequestBodyLBS request_body request'

  (\response -> getResponseBody response :: AccessToken) <$> httpJSON request
