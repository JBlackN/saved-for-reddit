{-|
Module     : Text.Blaze.Html5.Attributes.Extra
Description: Extra attributes
             for [blaze-html](https://hackage.haskell.org/package/blaze-html)
Copyright  : (c) Petr Schmied, 2018
License    : MIT
Maintainer : peter9209@gmail.com
Stability  : stable
Portability: portable

Module defines extra attributes
for [blaze-html](https://hackage.haskell.org/package/blaze-html).
-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Blaze.Html5.Attributes.Extra where

import Data.String (fromString)
import Text.Blaze.Internal (Attribute, AttributeValue, attribute)

-- | Generates @crossorigin@ attribute for @\<link\>@ and @\<script\>@ elements
-- (among others).
--
-- @See:@
-- <https://developer.mozilla.org/en-US/docs/Web/HTML/CORS_settings_attributes>.
crossorigin :: AttributeValue -> Attribute
crossorigin = attribute "crossorigin" " crossorigin=\""

-- | Generates @integrity@ attribute for @\<link\>@ and @\<script\>@ elements.
--
-- @See:@
-- <https://developer.mozilla.org/en-US/docs/Web/Security/Subresource_Integrity>.
integrity :: AttributeValue -> Attribute
integrity = attribute "integrity" " integrity=\""

-- | Generates @role@
--  [ARIA](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA)
-- attribute.
--
-- @See:@
-- <https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/ARIA_Techniques/Using_the_button_role>.
role :: AttributeValue -> Attribute
role = attribute "role" " role=\""

-- | Generates HTML5
--  [data-*](https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/data-*)
-- attributes.
data_ :: String -- ^ Attribute name (without the @"data-"@ part).
      -> AttributeValue
      -> Attribute
data_ key = attribute (
    fromString ("data-" ++ key)
  ) (
    fromString (" data-" ++ key ++ "=\"")
  )
