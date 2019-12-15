module Parse.Type where

import Data.Char
import Text.Parsec

type Parser = Parsec String ()

mkLink :: String -> String
mkLink = map replace
  where replace ' ' = '-'
        replace c   = toLower c
