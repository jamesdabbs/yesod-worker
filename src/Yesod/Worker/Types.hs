module Yesod.Worker.Types where

import Control.Concurrent (MVar)
import Database.Redis     (Connection)
import Keenser            (Manager)
import Yesod              (Yesod)
import Yesod.Core         (WidgetT, HandlerT, Html, defaultLayout)

data Workers = Workers
  { wManager :: MVar Manager
  , wRedis   :: Connection
  }

class Yesod master => YesodWorker master where
  workers :: master -> Workers

  workerLayout :: WidgetT master IO () -> HandlerT master IO Html
  workerLayout = defaultLayout

