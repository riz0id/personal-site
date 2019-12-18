{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}

module Webpages.Index (IxAPI, ixPage) where

import Servant
import Servant.HTML.Blaze

import           Text.Blaze.Html5               (Html, (!))
import qualified Text.Blaze.Html5            as B
import           Text.Blaze.Html5.Attributes 

import qualified Templates
import Type

type IxAPI =
       "style" :> Raw
  :<|> "home"  :> Get '[HTML] B.Html
  
ixPage = (serveDirectoryFileServer "./static/css")
    :<|> (Templates.index "rizoid.space" $ do
           B.section $ do
             B.p "I am a functional programmer with a keen interest in space flight. I love to study applied computational physics, formal systems and geometry. feel free to contact me via twitter or by email."
             B.ul $ do
               B.li $ B.a ! href "https://twitter.com/riz0id"
                          $      "twitter"
               B.li $ B.a ! href "https://github.com/riz0id"
                          $      "github")
                   
