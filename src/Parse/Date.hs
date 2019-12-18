{-# LANGUAGE OverloadedStrings #-}

module Parse.Date (Date, date) where

import Control.Monad
import Parse.Type
import Text.Parsec

import           Text.Blaze.Html5               (ToMarkup, (!))
import qualified Text.Blaze.Html5            as B
import qualified Text.Blaze.Html5.Attributes as A

year :: Parser String
year = replicateM 4 digit 

month :: Parser String
month = many1 digit 

day :: Parser String
day = many1 digit

data Date = Date String String String
          
date :: Parser Date
date = Date
       <$> (year  <* char '-' <?> error "/date/ missing year")
       <*> (month <* char '-' <?> error "/date/ missing month")
       <*> (day   <* newline  <?> error "/date/ trailing whitespace")

instance ToMarkup Date where
  toMarkup (Date y m d) = B.h3 $ B.toHtml (y ++ "-" ++ m ++ "-" ++ d)
