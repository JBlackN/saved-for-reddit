{-# LANGUAGE OverloadedStrings #-}
module Text.Blaze.Html5.Attributes.Extra where

import Data.String (fromString)
import Text.Blaze.Internal (Attribute, AttributeValue, attribute)

crossorigin :: AttributeValue -> Attribute
crossorigin = attribute "crossorigin" " crossorigin=\""

integrity :: AttributeValue -> Attribute
integrity = attribute "integrity" " integrity=\""

role :: AttributeValue -> Attribute
role = attribute "role" " role=\""

data_ :: String -> AttributeValue -> Attribute
data_ key = attribute (
    fromString ("data-" ++ key)
  ) (
    fromString (" data-" ++ key ++ "=\"")
  )
