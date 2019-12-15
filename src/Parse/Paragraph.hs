{-# LANGUAGE GADTs #-}

module Parse.Paragraph (Paragraph, paragraph) where

import Text.Parsec
import Parse.Type
import Parse.Code
  
import           Text.Blaze.Html5               (ToMarkup, toHtml, (!))
import qualified Text.Blaze.Html5            as B
import qualified Text.Blaze.Html5.Attributes as A
 
data TextElements where
  -- markdowns
  Bold   :: TextElements -> TextElements
  Italic :: TextElements -> TextElements
  Under  :: TextElements -> TextElements

  Linebreak :: TextElements

  Atom   :: String       -> TextElements
  Inline :: String       -> TextElements

inlineMd :: Parser a -> (Parser TextElements -> Parser TextElements)
inlineMd tok = between tok tok  

bold   = return . Bold   =<< inlineMd (string "**") (atom <|> italic)
italic = return . Italic =<< inlineMd (char '*' <|> char '_') (atom <|> bold)
under  = return . Under  =<< inlineMd (string "__") atom
inline = return . Inline =<< between (char '`') (char '`') (many1 $ noneOf "`")
linebreak = Linebreak <$ newline
atom   = return . Atom   =<< many1 (satisfy syntax) 
         where syntax c =  c /= '`'
                        && c /= '_'
                        && c /= '*'
                        && c /= '\n'

data Paragraph = Paragraph [TextElements]
               
paragraph :: Parser Paragraph
paragraph = manyTill syntax newline >>= return . Paragraph
  where syntax = linebreak <|> atom <|> bold <|> under <|> italic <|> inline

instance ToMarkup TextElements where
  toMarkup (Atom   inner) =          toHtml   inner
  toMarkup (Bold   inner) = B.b    . toHtml $ inner
  toMarkup (Italic inner) = B.i    . toHtml $ inner
  toMarkup (Under  inner) = B.u    . toHtml $ inner
  toMarkup (Inline inner) = B.code . toHtml $ inner
                            
instance ToMarkup Paragraph where
  toMarkup (Paragraph inners) = B.p $ foldl1 (>>) (map B.toHtml inners)
