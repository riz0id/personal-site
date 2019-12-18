{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Webpages.Summary where

import Control.Monad.Trans.Reader

import Servant
import Servant.HTML.Blaze

import           Text.Blaze.Html5 (toHtml, Html, (!))
import qualified Text.Blaze.Html5             as B
import qualified Text.Blaze.Html5.Attributes  as A

import qualified Templates
  
import Type

type PostsAPI = "posts" :> Get '[HTML] Html
  
postSummaries :: ConfigM Html
postSummaries = do
  WebConfig { dateRevised = _
            , postCorpus  = markdowns } <- ask

  let markups = foldr1 (\x y -> x >> B.br
                             >> (B.hr ! A.class_ "pagniation")
                             >> y) 
                $ map Templates.preview markdowns

  Templates.index "rizoid.space" markups
