module Type where

import Control.Monad.Trans.Reader
import Data.Time.Clock
import Servant

data WebConfig = WebConfig {
    dateRevised :: UTCTime
  , postCorpus  :: [String]
  }

type ConfigM = ReaderT WebConfig Handler
