module Templates.Type where

import           Control.Monad.Trans.Reader
import           Data.Time.Clock
import           Servant

data WebConfig = WebConfig
  { dateRevised :: UTCTime
  , navigation  :: [String]
  }

type ConfigM = ReaderT WebConfig Handler
