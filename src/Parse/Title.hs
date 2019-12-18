{-# LANGUAGE OverloadedStrings #-}

module Parse.Title (Title, title, mkReadmore) where

import Data.Char
import Parse.Type
import Text.Parsec
  
import           Text.Blaze.Html5               (ToMarkup, (!))
import qualified Text.Blaze.Html5            as B
import qualified Text.Blaze.Html5.Attributes as A

data Title = Title String

title :: Parser Title
title = manyTill anyChar newline
      >>= return . Title

instance ToMarkup Title where
  toMarkup (Title inner) = B.h3 . B.toHtml $ inner

mkReadmore :: Title -> B.Html
mkReadmore (Title inner) =
    B.a ! A.href (B.stringValue $ "/" ++ mkLink inner)
  $ "read more →"

