{-|
Module     : SfR.Metadata
Description: Web application's metadata
Copyright  : (c) Petr Schmied, 2018
License    : MIT
Maintainer : peter9209@gmail.com
Stability  : stable
Portability: portable

Module defines methods for web application's metadata management.
-}
module SfR.Metadata where

import Data.Version (showVersion)
import qualified Paths_saved_for_reddit as App (version)

-- | Web application's version.
--
-- Matches version in @package.yaml@.
version :: String
version = showVersion App.version

-- | @User-Agent@ HTTP header value for the web application.
--
-- In the form of @haskell:saved-for-reddit:v@'version'@ (by \/u\/JBlackN)@.
userAgent :: String
userAgent = "haskell:saved-for-reddit:v" ++ version ++ " (by /u/JBlackN)"
