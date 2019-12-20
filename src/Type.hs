module Type where

import           Control.Monad.Trans.Reader
import           Data.Time.Clock
import           Servant

newtype WebConfig = WebConfig {
    dateRevised :: UTCTime
  }

type ConfigM = ReaderT WebConfig Handler
