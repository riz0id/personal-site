{-# LANGUAGE OverloadedStrings #-}

module Templates.Index (index) where

import Control.Monad.Trans.Reader
import Data.Time

import qualified Text.Blaze.Html5 as B
import Text.Blaze.Html5 (Html, (!))
import Text.Blaze.Html5.Attributes
import Text.Blaze.Renderer.String (renderMarkup)

import Type

index :: Html -> Html -> ConfigM Html
index pageTitle innerContent = do
  WebConfig { dateRevised = date
            , postCorpus  = _ } <- ask
  
  return $ B.docTypeHtml ! lang "en" $ do 
    B.head $ do
      B.meta  ! charset "utf-8"
      B.title $ pageTitle 
      stylesheet "style/index.css"
      
    B.body $ do 
      B.header logo
      nav [ "home", "posts" ]
      B.hr
       
      B.main innerContent

      B.br
      B.footer $ do
        B.span . B.toHtml $ "last revised on " ++ (showGregorian . utctDay) date
        B.span $ do
            "written in "
            B.a ! href "https://github.com/riz0id/personal-site"
                $ "haskell"
          
stylesheet :: B.AttributeValue -> Html
stylesheet link =
  B.link ! rel   "stylesheet"
         ! type_ "text/css"
         ! href  link

logo :: Html
logo = B.mark . B.h3 $ "rizoid.space"

nav :: [B.Html] -> Html
nav xs = B.nav $ mconcat $ map navOpt xs
  where navOpt x =
            B.a ! href (B.stringValue $ renderMarkup x)
          $ B.h3 x
