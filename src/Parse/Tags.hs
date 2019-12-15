module Parse.Tags (Tags, tags) where

import Text.Parsec
import Parse.Type
    
import           Text.Blaze.Html5               (ToMarkup, (!))
import qualified Text.Blaze.Html5            as B
import qualified Text.Blaze.Html5.Attributes as A

data Tags = Tags [String]

tag :: Parser String
tag = manyTill anyChar newline <* spaces

tags :: Parser Tags
tags = tag `sepBy` (char '-')
     >>= return . Tags

instance ToMarkup Tags where
  toMarkup (Tags ts) = foldl1 (>>) $ map tagToMarkup ts
    where tagToMarkup t =
              B.a ! A.href (B.stringValue $ "search?=" ++ mkLink t)
            $ B.li
            $ B.b
            $ B.toHtml t

