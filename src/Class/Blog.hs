{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Class.Blog where

import           Text.Blaze.Html5 (Html)

import           API

class IsBlog md where
  blogIx :: IO Html

  blogPost :: IO Html
  
