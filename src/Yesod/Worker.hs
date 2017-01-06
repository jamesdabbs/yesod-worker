{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
module Yesod.Worker
  ( Worker(..)
  , Workers
  , YesodWorker(..)
  , bootWorkers
  , bootManagers
  , newWorkers
  , concurrency
  , register
  , enqueue
  , enqueueAt
  , enqueueIn
  , workerRoute
  ) where

import Keenser hiding (enqueue, enqueueAt, enqueueIn)
import Yesod.Worker.Site
import Yesod.Worker.Types
import Yesod.Worker.Util
import Yesod

import Control.Concurrent
import Control.Monad
import Database.Redis

instance YesodWorker master => YesodSubDispatch Workers (HandlerT master IO) where
  yesodSubDispatch = $(mkYesodSubDispatch resourcesWorkers)

newWorkers :: ConnectInfo -> IO Workers
newWorkers c = Workers <$> newEmptyMVar <*> connect c


bootManagers :: YesodWorker master => Configurator (HandlerT master IO) -> HandlerT master IO ()
bootManagers declareJobs = void $ do
  Workers{..} <- workers <$> getYesod

  -- TODO: need to ensure we have enough connections in the pool to cover
  -- - however many connections the client might need
  conn <- liftIO $ connect (defaultConnectInfo { connectMaxConnections = 100 })
  conf <- mkConf conn $ do
    concurrency 10
    middleware record
    middleware retry
    declareJobs

  m <- mkManager conf
  liftIO $ putMVar wManager m


bootWorkers :: YesodWorker master => Configurator (HandlerT master IO) -> HandlerT master IO ()
bootWorkers declareJobs = void $ do
  master <- getYesod

  let Workers{..} = workers master

  -- TODO: need to ensure we have enough connections in the pool to cover
  -- - concurrency many workers
  -- - the heartbeat process
  -- - the signal listener
  -- - however many connections the client might need
  conn <- liftIO $ connect ((redisConfig master) { connectMaxConnections = 100 })

  conf <- mkConf conn $ do
    concurrency 10
    middleware record
    middleware retry
    declareJobs

  forkHandler handleError $ do
    manager <- startProcess conf
    liftIO $ do
      putMVar wManager manager
      forever $ sleep 60

handleError :: e -> HandlerT site IO ()
handleError _ = $(logError) "handleError"

workerRoute = HomeR
