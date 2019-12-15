
module Parse.Code (Code, code) where

import Parse.Type
import Text.Parsec

-- TODO: this
data Code = Code String

code :: Parser Code
code = between (string "```") (string "```") (many1 anyChar)
       >>= return . Code
