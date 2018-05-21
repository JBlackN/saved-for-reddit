{-|
Module     : SfR.Reddit.Types
Description: Reddit user and access token data types
Copyright  : (c) Petr Schmied, 2018
License    : MIT
Maintainer : peter9209@gmail.com
Stability  : stable
Portability: portable

Module defines data types for [Reddit](https://www.reddit.com) user and access
token.
-}
{-# LANGUAGE DeriveGeneric #-}
module SfR.Reddit.Types where

import Data.Aeson
import GHC.Generics

-- | [Reddit](https://www.reddit.com) access token with metadata.
--
-- Does not store refresh token &#x2013; application doesn't use it.
--
-- See
-- <https://github.com/reddit-archive/reddit/wiki/OAuth2#retrieving-the-access-token>.
data AccessToken = AccessToken { -- | [Reddit](https://www.reddit.com) API
                                 --   access token.
                                 access_token :: String
                                 -- | @"bearer"@.
                               , token_type   :: String
                                 -- | Unix epoch seconds.
                               , expires_in   :: Integer
                                 -- | Scopes accessible using this token. See
                                 --   <https://www.reddit.com/dev/api/oauth>.
                               , scope        :: String
                               } deriving (Generic, Show)

-- | [Reddit](https://www.reddit.com) user.
newtype RedditUser = RedditUser { -- | [Reddit](https://www.reddit.com)
                                  --   username.
                                  name :: String
                                } deriving (Generic, Show)

-- | Extract [Reddit](https://www.reddit.com) API access token from JSON.
instance FromJSON AccessToken
-- | Extract [Reddit](https://www.reddit.com) username from JSON.
instance FromJSON RedditUser
