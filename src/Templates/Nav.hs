module Templates.Nav where

import           Control.Monad.Trans.Reader

import           Text.Blaze.Html5            (Html, (!))
import qualified Text.Blaze.Html5            as B
import qualified Text.Blaze.Html5.Attributes as A

import           Templates.Type

nav :: ConfigM Html
nav = do
  let mkLink x = B.a ! A.href (B.stringValue x)
                     $ B.toHtml x

  config <- ask
  return . B.nav
         $ mconcat
         $ map mkLink (navigation config)
