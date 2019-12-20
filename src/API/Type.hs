{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}

module API.Type where

import           GHC.TypeLits

import           Servant
import           Servant.HTML.Blaze
import           Text.Blaze.Html5   (Html)

type Webpage (path :: Symbol) = path :> Get '[HTML] Html
