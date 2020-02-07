-# LANGUAGE OverloadedStrings #-}

module Templates.Footer where

import           Control.Monad.Trans.Reader
import           Data.Time

import           Text.Blaze.Html5            (Html, (!))
import qualified Text.Blaze.Html5            as B
import qualified Text.Blaze.Html5.Attributes as A

import           Templates.Type

footer :: ConfigM Html
footer = do
  config <- ask
  let date = (showGregorian . utctDay) (dateRevised config)

  return . B.footer $ do
    B.p $ do
      "last revised: "
      B.time (B.toHtml date)
    B.p $ do
      "written in "
      B.a ! A.href "https://github.com/riz0id/personal-site"
          $ "haskell"
