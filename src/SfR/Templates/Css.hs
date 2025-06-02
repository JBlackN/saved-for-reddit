{-|
Module     : SfR.Templates.Css
Description: Web application's CSS stylesheets
Copyright  : (c) Petr Schmied, 2018
License    : MIT
Maintainer : peter9209@gmail.com
Stability  : stable
Portability: portable

Module defines web application's CSS stylesheets.
-}
{-# LANGUAGE OverloadedStrings #-}
module SfR.Templates.Css where

import Clay
import Prelude hiding (rem)

-- | Web application's CSS stylesheets.
-- All custom styles have been removed in favor of Tailwind CSS utility classes.
-- This can be used for any remaining global styles or very specific component styles
-- not easily covered by Tailwind.
css :: Css
css = do
  -- Example of how you might add a very specific style if needed:
  -- query "h1.specific-legacy-class" Clay.? do
  --   color red
  return () -- Ensures the Css block is valid if completely empty
