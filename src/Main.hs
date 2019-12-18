{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

import Data.Maybe         (fromMaybe)
import System.Environment (getArgs, lookupEnv)
import Text.Read          (readMaybe)
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

import qualified Network.Wai.Handler.Warp as Warp
import           Network.Wai                 (Application)
import           Network.HTTP.Client         (defaultManagerSettings, newManager)
import qualified Text.Blaze.Html5   as B
import Data.Time.Clock

import Servant
import Servant.HTML.Blaze
import Servant.Server.StaticFiles (serveDirectoryFileServer)

import Webpages
import Type

nt :: WebConfig -> ConfigM a -> Handler a
nt s x = runReaderT x s


type BlogAPI = "posts" :</> "posts/Test Post2.md"
                       ://  "posts/Test Post1.md"

type API =    IxAPI
         :<|> PostAPI BlogAPI

api :: Proxy API
api = Proxy

server :: ServerT API ConfigM 
server = ixPage :<|> return (reifyPosts @BlogAPI)
       

app :: WebConfig -> Application
app s = serve api $ hoistServer api (nt s) server

main :: IO ()
main = do
  args <- getArgs
  mgr  <- newManager defaultManagerSettings
  port <- fmap (fromMaybe 8000 . (>>= readMaybe)) (lookupEnv "PORT")

  -- get current build date for revised date on website
  date <- getCurrentTime

  -- website config
  config <- return WebConfig {
                dateRevised = date
              , postCorpus  = []
              }

  putStrLn $ "running on http://localhost:" ++ show port ++ "/"
  Warp.run port $ app $ config

