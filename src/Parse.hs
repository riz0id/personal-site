module Parse
  ( P.mkReadmore
  , parsePost, Blogpost(..)
  ) where

import Text.Parsec

import qualified Parse.Title     as P  
import qualified Parse.Date      as P
import qualified Parse.Paragraph as P
import qualified Parse.Desc      as P
import qualified Parse.Body      as P
import           Parse.Type

data Blogpost = Blogpost {
    titleFrom :: P.Title 
  , dateFrom  :: P.Date 
  , descFrom  :: P.Desc
  , bodyFrom  :: [P.Body]
  }

parsePost :: Parser Blogpost
parsePost = do
  title <- string "title:"     *> spaces *> P.title <* spaces
  date  <- string "date:"      *> spaces *> P.date  <* spaces
  desc  <- string "post-desc:" *> spaces *> P.desc
  body  <- string "post-body:" *> spaces *> P.body
  return $ Blogpost title date desc body
 
