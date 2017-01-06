module Yesod.Worker.Types where

import Control.Concurrent (MVar)
import Database.Redis     (Connection, ConnectInfo, defaultConnectInfo)
import Keenser            (Manager)
import Yesod              (Yesod)
import Yesod.Core         (WidgetT, HandlerT, Html, defaultLayout)

data Workers = Workers
  { wManager :: MVar Manager
  , wRedis   :: Connection
  }

class Yesod master => YesodWorker master where
  workers :: master -> Workers

  redisConfig :: master -> ConnectInfo
  redisConfig _ = defaultConnectInfo

  workerLayout :: WidgetT master IO () -> HandlerT master IO Html
  workerLayout = defaultLayout