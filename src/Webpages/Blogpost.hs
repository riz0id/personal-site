{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}

module Webpages.Blogpost
  ( type (://)(..)
  , PostAPIs, PostAPI
  , reifyPosts
  , (:</>)(..)
  ) where

import GHC.TypeLits
import Data.Typeable
import Control.Monad.IO.Class
import Control.Monad
  
import Servant 
import Servant.HTML.Blaze
import qualified Text.Blaze.Html5   as B
import qualified Text.Parsec.String as P

import           Text.Blaze.Html5 (toHtml, Html, (!))
import qualified Text.Blaze.Html5             as B
import qualified Text.Blaze.Html5.Attributes  as A

import qualified Templates
import Type
import Parse

-- generates blogposts from markdowns and their urls
--
infix 4 :</>
data (dir :: Symbol) :</> posts
  deriving Typeable

infixr 5 :// 
data (path :: Symbol) :// (paths :: k)
  deriving Typeable

class PostAPIs ps where
  type PostAPI ps

  postLinks  :: IO [Blogpost]

  reifyPosts :: IO (ConfigM Html)
  reifyPosts = do 
    markdowns <- postLinks @ps
    let markups = map Templates.preview markdowns
    return (Templates.index "lala" (mconcat markups))
                  
instance (KnownSymbol p, PostAPIs ps) => PostAPIs (p :// ps) where
  type PostAPI (p :// ps) = p :> Get '[HTML] B.Html :<|> PostAPI ps
    
  postLinks = do
    blogpost  <- getBlogpost . symbolVal $ Proxy @p
    blogposts <- postLinks @ps 
    return $ blogpost : blogposts
  
instance KnownSymbol p => PostAPIs p where
  type PostAPI p = p :> Get '[HTML] B.Html
    
  postLinks = sequence (filepath : [])
    where filepath = getBlogpost . symbolVal $ Proxy @p

getBlogpost :: String -> IO Blogpost
getBlogpost filepaths = do
  maybeBp <- P.parseFromFile parsePost filepaths
  return $ case maybeBp of
    Left err -> error . show $ err
    Right bp -> bp
