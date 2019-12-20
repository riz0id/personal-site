{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Index (Index, index) where

import           Servant
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
  B.p "I am a functional programmer with a keen interest in space flight. I love to study applied computational physics, formal systems and geometry. feel free to contact me via twitter or by email."
  B.ul $ do
    B.li $ B.a ! href "https://twitter.com/riz0id"
               $      "twitter"
    B.li $ B.a ! href "https://github.com/riz0id"
               $      "github"
