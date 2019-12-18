module Type where

import Control.Monad.Trans.Reader
import Data.Time.Clock
import Servant

import Parse (Blogpost)

data WebConfig = WebConfig {
    dateRevised :: UTCTime
  , postCorpus  :: [Blogpost]
  }

type ConfigM = ReaderT WebConfig Handler
