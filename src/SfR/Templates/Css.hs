{-# LANGUAGE OverloadedStrings #-}
module SfR.Templates.Css where

import Clay
import Prelude hiding (rem)

css :: Css
css = do
  html ? height (pct 100)
  body ? height (pct 100)
  ".saved-item-title" ? fontSize (rem 1.25)
  p ? textAlign justify
