{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module API where

import           Servant

import qualified API.Index      as API
import qualified API.Resume     as API
import           Templates.Type

type SiteAPI =    API.Index
             :<|> API.Resume
             :<|> "static" :> Raw
             :<|> "style"  :> Raw

siteApi :: Proxy SiteAPI
siteApi = Proxy

server :: ServerT SiteAPI ConfigM
server =    API.index
       :<|> API.resume
       :<|> serveDirectoryFileServer "./static"
       :<|> serveDirectoryFileServer "./css"
