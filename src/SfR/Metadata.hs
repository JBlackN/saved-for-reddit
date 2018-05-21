module SfR.Metadata where

import Data.Version (showVersion)
import qualified Paths_saved_for_reddit as App (version)

version :: String
version = showVersion App.version

userAgent :: String
userAgent = "haskell:saved-for-reddit:v" ++ version ++ " (by /u/JBlackN)"
