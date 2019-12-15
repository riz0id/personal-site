{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

import Data.Maybe         (fromMaybe)
import System.Environment (getArgs, lookupEnv)
import Text.Read          (readMaybe)
import Control.Monad.Trans.Reader

import qualified Network.Wai.Handler.Warp as Warp
import           Network.Wai                 (Application)
import           Network.HTTP.Client         (defaultManagerSettings, newManager)
import qualified Text.Blaze.Html5 as B

import Data.Time.Clock

import Servant
import Servant.HTML.Blaze
import Servant.Server.StaticFiles (serveDirectoryFileServer)

import qualified Templates

import qualified Webpages.Index  as Webpages
import qualified Webpages.Posts  as Webpages
import Type

nt :: WebConfig -> ConfigM a -> Handler a
nt s x = runReaderT x s
         
type API = Webpages.IxAPI

api :: Proxy API
api = Proxy

server :: ServerT API ConfigM 
server =    Webpages.ixPage
       :<|> serveDirectoryFileServer "./static/css"

app :: WebConfig -> Application
app s = serve api $ hoistServer api (nt s) server

main :: IO ()
main = do
  args   <- getArgs
  mgr    <- newManager defaultManagerSettings
  port   <- fmap (fromMaybe 8000 . (>>= readMaybe)) (lookupEnv "PORT")
  date   <- getCurrentTime
  config <- return WebConfig {
                dateRevised = date
              , postCorpus  = [ "./posts/testPost.txt" ]
              }

  putStrLn $ "running on http://localhost:" ++ show port ++ "/"
  Warp.run port $ app $ config

