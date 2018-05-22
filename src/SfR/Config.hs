{-|
Module     : SfR.Config
Description: Web application's configuration
Copyright  : (c) Petr Schmied, 2018
License    : MIT
Maintainer : peter9209@gmail.com
Stability  : stable
Portability: portable

Module defines methods for web application's configuration management.
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module SfR.Config where

import Data.Either
import Data.Yaml
import GHC.Generics

-- | __Saved for Reddit__ web application's configuration.
data SfRConfig = SfRConfig { -- | Web application's OAuth2 callback route path.
                             callback_path :: String
                             -- | OAuth2 callback URL.
                           , callback_url :: String
                             -- | Web application's client ID.
                           , client_id :: String
                             -- | Web application's client secret.
                           , client_secret :: String
                             -- | Path to [SQLite](https://www.sqlite.org)
                             --   database file.
                           , db_file :: String
                             -- | Web application's port.
                           , port :: Int
                             -- | Whether to use session only when using HTTPS.
                           , secure_cookie :: Bool
                           } deriving (Generic, Show)
-- | Extract 'SfRConfig' from JSON.
instance FromJSON SfRConfig

-- | Gets __Saved for Reddit__ web application's configuration.
--
-- Attempts to load configuration ('SfRConfig') from @.\/config.yml@ file.
sfrConfig :: IO SfRConfig
sfrConfig = either (error . show) id <$> decodeFileEither "config.yml"
