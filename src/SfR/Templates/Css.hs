{-# LANGUAGE OverloadedStrings #-}
module SfR.Templates.Css where

import Clay

css :: Css
css = do
  html ? height (pct 100)
  body ? height (pct 100)
