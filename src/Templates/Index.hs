{-# LANGUAGE OverloadedStrings #-}

module Templates.Index where

import           Text.Blaze.Html5            (Html, (!))
import qualified Text.Blaze.Html5            as B
import           Text.Blaze.Html5.Attributes

import qualified Templates.Footer            as Templates
import qualified Templates.Header            as Templates
import qualified Templates.Nav               as Templates

import           Templates.Type

index :: Html -> ConfigM Html
index innerContent = do
  nav    <- Templates.nav
  footer <- Templates.footer

  return . B.docTypeHtml $ do
    B.head $ do
      B.meta ! charset "utf-8"
      B.link ! rel   "stylesheet"
             ! type_ "text/css"
             ! href  "style/style.css"

      B.title "rizz.space"

    B.body $ do
      Templates.header
      nav
      B.main innerContent
      footer
