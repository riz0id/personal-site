module Parse.Desc (Desc, desc) where

import Parse.Type
import Parse.Paragraph

import           Text.Blaze.Html5               (ToMarkup, (!))
import qualified Text.Blaze.Html5            as B
import qualified Text.Blaze.Html5.Attributes as A

import Text.Parsec

data Desc = Desc [Paragraph]
          
desc :: Parser Desc
desc = manyTill (paragraph <* spaces) (try . lookAhead $ string "post-body:")
       >>= return . Desc

instance ToMarkup Desc where
  toMarkup (Desc inners) = foldl1 (>>) (map B.toHtml inners)
