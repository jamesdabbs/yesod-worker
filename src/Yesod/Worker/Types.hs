module Yesod.Worker.Types where

import Control.Concurrent (MVar)
import Database.Redis     (Connection)
import Keenser            (Manager)
import Yesod              (Yesod)

data Workers = Workers
  { wManager :: MVar Manager
  , wRedis   :: Connection
  }

class Yesod master => YesodWorker master where
  workers :: master -> Workers
