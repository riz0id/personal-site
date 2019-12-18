module Templates.Post ( preview ) where

import Data.Char
import Data.Maybe

import Text.Blaze.Html5 (ToMarkup, Html, toHtml)
import qualified Text.Blaze.Html5 as B

import Parse

preview :: Blogpost -> Html
preview blogmd = do
  B.section $ do
    B.header $ do
      toHtml . titleFrom $ blogmd
      toHtml . dateFrom  $ blogmd
    B.hr
    B.summary $ do
      toHtml . descFrom $ blogmd
    B.footer $ do
      mkReadmore . titleFrom $ blogmd
