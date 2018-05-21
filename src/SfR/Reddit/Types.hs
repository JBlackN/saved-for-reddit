{-# LANGUAGE DeriveGeneric #-}
module SfR.Reddit.Types where

import Data.Aeson
import GHC.Generics

data AccessToken = AccessToken { access_token :: String
                               , token_type   :: String
                               , expires_in   :: Integer
                               , scope        :: String
                               } deriving (Generic, Show)

newtype RedditUser = RedditUser { name :: String
                                } deriving (Generic, Show)

instance FromJSON AccessToken
instance FromJSON RedditUser
