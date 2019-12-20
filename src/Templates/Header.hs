{-# LANGUAGE OverloadedStrings #-}

module Templates.Header where

import           Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as B

header :: Html
header = B.header . B.mark $ "rizoid.space"
