module Parse.Header (Header, header) where

import Text.Parsec
import Parse.Type
    
import           Text.Blaze.Html5               (ToMarkup, (!))
import qualified Text.Blaze.Html5            as B
import qualified Text.Blaze.Html5.Attributes as A

data Header = H1 String
            | H2 String
            | H3 String
            | H4 String
            | H5 String
            | H6 String

takeHeader :: Parser String
takeHeader = manyTill (alphaNum <|> space) newline

tryHeader :: String -> Parser String
tryHeader str = string str *> spaces *> takeHeader
                
header :: Parser Header
header =
      (tryHeader "#"      >>= return . H1)
  <|> (tryHeader "##"     >>= return . H2)
  <|> (tryHeader "###"    >>= return . H3)
  <|> (tryHeader "####"   >>= return . H4)
  <|> (tryHeader "#####"  >>= return . H5)
  <|> (tryHeader "######" >>= return . H6)

instance ToMarkup Header where
  toMarkup (H1 inner) = B.h1 . B.toHtml $ inner
  toMarkup (H2 inner) = B.h2 . B.toHtml $ inner
  toMarkup (H3 inner) = B.h3 . B.toHtml $ inner
  toMarkup (H4 inner) = B.h4 . B.toHtml $ inner
  toMarkup (H5 inner) = B.h5 . B.toHtml $ inner
  toMarkup (H6 inner) = B.h6 . B.toHtml $ inner
