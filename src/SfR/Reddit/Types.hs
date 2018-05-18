{-# LANGUAGE DeriveGeneric #-}
module SfR.Reddit.Types where

import Data.Aeson
import GHC.Generics

data AccessToken = AccessToken { access_token :: String
                               , token_type   :: String
                               , expires_in   :: Integer
                               , scope        :: String
                               } deriving (Generic, Show)

instance FromJSON AccessToken
