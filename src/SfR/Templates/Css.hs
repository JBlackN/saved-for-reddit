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
  -- CSS Variables for Light Theme (default)
  ":root" ? do
    custom "--primary-bg-color" "#ffffff"
    custom "--secondary-bg-color" "#f8f9fa"
    custom "--text-color" "#212529"
    custom "--link-color" "#007bff"
    custom "--border-color" "#dee2e6"

  -- Dark Theme using media query
  query media [Only screen [Feature "prefers-color-scheme" (Just "dark")]] $ do
    ":root" ? do
      custom "--primary-bg-color" "#121212"
      custom "--secondary-bg-color" "#1e1e1e"
      custom "--text-color" "#e0e0e0"
      custom "--link-color" "#64b5f6"
      custom "--border-color" "#424242"

    -- Specific dark theme overrides
    ".text-muted" ? color "#a0a0a0"

  html ? height (pct 100)
  body ? do
    height (pct 100)
    backgroundColor (var "--primary-bg-color")
    color (var "--text-color")
  ".saved-item-title" ? fontSize (rem 1.25)
  p ? textAlign justify

  -- Navbar styling
  ".navbar.navbar-light.bg-light" ? do
    backgroundColor (var "--secondary-bg-color")
    borderColor (var "--border-color")

  ".navbar .navbar-brand" ? do
    color (var "--text-color")

  ".navbar .nav-link" ? do
    color (var "--link-color")

  -- Card styling
  ".card" ? do
    backgroundColor (var "--primary-bg-color")
    color (var "--text-color")
    borderColor (var "--border-color")

  -- List group item styling
  ".list-group-item" ? do
    backgroundColor (var "--primary-bg-color")
    color (var "--text-color")
    borderColor (var "--border-color")
