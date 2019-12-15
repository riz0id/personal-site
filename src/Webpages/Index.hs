{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}

module Webpages.Index (IxAPI, ixPage) where

import Servant
import Servant.HTML.Blaze
import Servant.Server.StaticFiles (serveDirectoryFileServer)

import           Text.Blaze.Html5               (Html, (!))
import qualified Text.Blaze.Html5            as B
import           Text.Blaze.Html5.Attributes 

import qualified Templates
import Type

type IxAPI =
       "index" :> Get '[HTML] B.Html
  :<|> "style" :> Raw
  
ixPage :: ConfigM Html
ixPage = Templates.index "rizoid.space" $ do
  B.div $ do
    B.p "i am a functional programmer with a keen interest in space travel. i love to study applied computational physics, formal systems and geometry."

    B.h1 $ B.b "links"
    B.ul $ do
      B.li $ B.a ! href "https://twitter.com/riz0id"
                 $      "https://twitter.com/riz0id"
      B.li $ B.a ! href "https://github.com/riz0id"
                 $      "https://github.com/riz0id"
                   
