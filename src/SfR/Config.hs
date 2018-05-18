{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module SfR.Config where

import Data.Either
import Data.Yaml
import GHC.Generics

data SfRConfig = SfRConfig { callback_url :: String
                           , client_id :: String
                           , client_secret :: String
                           , port :: Int
                           } deriving (Generic, Show)
instance FromJSON SfRConfig

sfr_config :: IO SfRConfig
sfr_config = either (error . show) id <$> decodeFileEither "config.yml"
