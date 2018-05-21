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
css :: Css
css = do
  html ? height (pct 100)
  body ? height (pct 100)
  ".saved-item-title" ? fontSize (rem 1.25)
  p ? textAlign justify
