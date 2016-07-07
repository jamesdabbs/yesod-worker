{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Yesod.Worker.Site where

import Control.Concurrent
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Database.Redis
import Keenser
import Text.Blaze (ToMarkup(..))
import Yesod
import Yesod.Worker.Types
import Yesod.Worker.Util

instance ToMarkup BSC.ByteString where
  toMarkup = toMarkup . BSC.unpack

type WorkerH a = forall master . YesodWorker master => HandlerT Workers (HandlerT master IO) a

mkYesodSubData "Workers" [parseRoutes|
/ HomeR GET
|]

getHomeR :: WorkerH Html
getHomeR = do
  Workers{..} <- getYesod
  mm <- liftIO $ tryReadMVar wManager
  case mm of
    Just m -> do
      ManagerStatus{..} <- checkStatus m wRedis
      let info    = Map.fromList sState
          get key = fromMaybe "" $ Map.lookup key info
      lift $ defaultLayout $(whamletFile "templates/home.hamlet")
    _ -> do
      $(logError) "No manager running"
      return "No manager running"
