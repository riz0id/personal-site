{-# LANGUAGE GADTs #-}

module Parse.Body (Body, body) where

import Text.Parsec

import Parse.Paragraph
import Parse.Code
import Parse.Type
import Parse.Header

data Body where
  Textblock :: Paragraph -> Body
  Codeblock :: Code      -> Body
  Section   :: Header    -> Body
               
body :: Parser [Body]
body = manyTill (choice
       [ code      >>= return . Codeblock
       , paragraph >>= return . Textblock
       , header    >>= return . Section
       ]) eof
