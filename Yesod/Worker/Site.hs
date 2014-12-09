{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Yesod.Worker.Site where

import Prelude
import Yesod
import Yesod.Worker.Types
import Yesod.Worker (spawnWorker, killWorker)

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

type H site = HandlerT Workers (HandlerT site IO) Html

activePool :: (MonadHandler m, HandlerSite m ~ Workers) => m (Set.Set ThreadId)
activePool = getYesod >>= liftIO . atomically . readTVar . workerPool

activeQueue :: (MonadHandler m, HandlerSite m ~ Workers) => m (Seq.Seq String)
activeQueue = getYesod >>= liftIO . atomically . readTVar . workerJobQueue

getWorkerHomeR :: YesodWorker site => H site
getWorkerHomeR = do
  currentPool <- activePool
  currentQueue <- activeQueue
  toParent <- getRouteToParent
  lift $ defaultLayout $(whamletFile "templates/home.hamlet")

postKillWorkerR :: YesodWorker site => H site
postKillWorkerR = do
  workers <- getYesod
  master  <- lift getYesod
  liftIO $ killWorker master workers
  redirect WorkerHomeR

postSpawnWorkerR :: YesodWorker site => H site
postSpawnWorkerR = do
  workers <- getYesod
  lift $ do
    master <- getYesod
    liftIO $ spawnWorker master workers
  redirect WorkerHomeR

instance YesodWorker master => YesodSubDispatch Workers (HandlerT master IO) where
  yesodSubDispatch = $(mkYesodSubDispatch resourcesWorkers)
