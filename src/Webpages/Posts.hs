{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Webpages.Posts where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader

import Servant
import Servant.HTML.Blaze

import Text.Blaze.Html5   (toHtml, Html)
import Text.Parsec.String (parseFromFile)

import qualified Templates
import qualified Webpages.Index
  
import Parse
import Type

type PostsAPI = "posts" :> Get '[HTML] Html
  
postSummaries :: ConfigM Html
postSummaries = do
  WebConfig { dateRevised = _
            , postCorpus  = filepaths } <- ask

  return . mconcat $ mapM getPostMarkup filepaths
  
getPostMarkup :: FilePath -> Html
getPostMarkup filepath =
  let blogpost = parseFromFile parsePost filepath
  in case blogpost of
       Left err -> error . show $ err
       Right markdown -> Templates.preview markdown
    
