{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module API where

import           Servant

import qualified API.Index      as API
import           Templates.Type

type SiteAPI =    API.Index
             :<|> "style" :> Raw

siteApi :: Proxy SiteAPI
siteApi = Proxy

server :: ServerT SiteAPI ConfigM
server =    API.index
       :<|> serveDirectoryFileServer "./css"

-- i have yet to do this because it's annoying and i need
-- to get this website finished
-- ╔════════════════════════════════════════════════════════╗
-- ║                      blog api example                  ║
-- ╠════════════════════════════════╦═══════════════════════╣
-- ║ "posts" <//> "posts/Post 1.md" ║ posts/ ─┬─ post-1     ║
-- ║         <&&> "posts/Post 2.md" ║         ├─ post-2     ║
-- ║         <&&> ...               ║         └─ ...        ║
-- ╚════════════════════════════════╩═══════════════════════╝
