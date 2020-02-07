{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Index (Index, index) where

import           Text.Blaze.Html5            (Html, (!))
import qualified Text.Blaze.Html5            as B
import           Text.Blaze.Html5.Attributes

import           API.Type
import           Templates                   (ConfigM)
import qualified Templates

type Index = Webpage "home"

index :: ConfigM Html
index = Templates.index $ do
  B.hr
  B.p "hi i'm jake! i'm a functional programmer and aspiring mathematician in texas. i mostly spend my free time studying type theory, constructive mathematics, numerical analysis, and philosophy."

  B.ul $ do
    B.li
      $ B.a ! href "mailto:rizoid@rizoid.space"
      $ "rizoid@rizoid.space"
    B.li
      $ B.a ! href "https://github.com/riz0id"
      $ "github"
    B.li
      $ B.a ! href "https://twitter.com/riz0id"
      $ "twitter"
