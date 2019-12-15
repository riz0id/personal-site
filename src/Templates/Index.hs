{-# LANGUAGE OverloadedStrings #-}

module Templates.Index (index) where

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.Time

import           Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as B
import           Text.Blaze.Html5.Attributes
import           Text.Blaze.Renderer.String (renderMarkup)

import Type

index :: B.Html -> B.Html -> ConfigM Html
index pageTitle innerContent = do
  WebConfig { dateRevised = date
            , postCorpus  = _ } <- ask
  
  return $ B.docTypeHtml ! lang "en" $ do 
    B.head $ do
      B.meta ! charset "utf-8"
      stylesheet "style/colors.css"
      stylesheet "style/index.css"
      stylesheet "style/post.css"
      B.title $ pageTitle
    B.body $ 
      B.div ! class_ "center container" $ do
        B.header ! class_ "header" $ do
          logo
          nav [ "home", "posts" ]
        B.div ! class_ "seperator" $ mempty
        innerContent
        B.footer $ do
          B.span $ B.toHtml $ "last revised on " ++ (showGregorian . utctDay) date
          B.span "written in haskell"
          
stylesheet :: B.AttributeValue -> Html
stylesheet link =
  B.link ! rel   "stylesheet"
         ! type_ "text/css"
         ! href  link

logo :: Html
logo = B.div ! class_ "header-logo"
     $ B.div ! class_ "logo"
     $ "rizoid.space"

nav :: [B.Html] -> Html
nav xs = B.nav
    $ B.ul ! class_ "nav"
    $ mconcat $ map navOpt xs
  where navOpt x =
            B.li
          $ B.a ! href (B.stringValue $ renderMarkup x)
          $ x
