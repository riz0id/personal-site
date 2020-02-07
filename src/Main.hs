{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

import           Control.Monad.Trans.Reader
import           Data.Maybe                 (fromMaybe)
import           Data.Time.Clock
import           System.Environment         (lookupEnv)
import           Text.Read                  (readMaybe)

import qualified Network.Wai.Handler.Warp   as Warp
import           Servant

import qualified API
import qualified Templates
import           Templates.Type

app :: WebConfig -> Application
app conf = serve API.siteApi
         $ hoistServer API.siteApi (`runReaderT` conf) API.server

main :: IO ()
main = do
  date <- getCurrentTime
  port <- fmap (fromMaybe 8000 . (>>= readMaybe)) (lookupEnv "PORT")

  let config = Templates.WebConfig
        { dateRevised = date
        , navigation  = ["home", "resume"]
        }

  Warp.run port $ app config
