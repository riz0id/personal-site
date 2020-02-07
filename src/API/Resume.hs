{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Resume (Resume, resume) where

import           Text.Blaze.Html5            (Html, (!))
import qualified Text.Blaze.Html5            as B
import           Text.Blaze.Html5.Attributes

import           API.Type
import           Templates                   (ConfigM)
import qualified Templates

type Resume = Webpage "resume"

resume :: ConfigM Html
resume = Templates.index $ do
  B.hr
  B.p $ do
    "you can find a pdf version " 
    B.a ! href "static/Resume.pdf"
        $ "here"
  B.iframe ! src "static/Resume.html" $ mempty
