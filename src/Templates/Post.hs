{-# LANGUAGE OverloadedStrings #-}

module Templates.Post
  ( preview, {-post-} ) where

import           Data.Char
import           Data.Maybe

import           Text.Blaze.Html5 (ToMarkup, Html, (!), toHtml)
import qualified Text.Blaze.Html5 as B
import           Text.Blaze.Html5.Attributes

import           Parse

preview :: Blogpost -> B.Html
preview blogmd = do
  B.div ! class_ "preview-body" $ do
    B.div ! class_ "preview-title" $ do
      toHtml . titleFrom $ blogmd
      toHtml . dateFrom  $ blogmd
    B.div ! class_ "post-content" $ do
      
      toHtml . descFrom $ blogmd
    B.div ! class_ "preview-footer" $ do
      B.ul  ! class_ "preview-tags" $ do
        B.toHtml . tagsFrom $ blogmd
      mkReadmore . titleFrom $ blogmd