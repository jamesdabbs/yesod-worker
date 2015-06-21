module Yesod.Worker
( defaultRunW
, enqueue
, emptyQueue
, getYesodW
, killWorker
, JobQueue
, startWorkers
, spawnWorker
, Workers
, WorkerT
, YesodWorker (..)
) where

import Prelude

import Yesod
import Yesod.Worker.Queue
import Yesod.Worker.Types

import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Monad (forever, liftM, replicateM_)
import Control.Monad.Trans.Resource (runResourceT, withInternalState)
import Control.Concurrent.STM (atomically, newTVar, modifyTVar, readTVar)
import Data.Monoid ((<>))
import qualified Data.Set as Set
import qualified Data.Text as Text
import Database.Persist.Sql (runSqlPool)


askWorkerEnv :: MonadWorker m => m (RunWorkerEnv (WorkerSite m))
askWorkerEnv = liftWorkerT $ WorkerT $ return . workerEnv

-- | Get the master site application argument.
getYesodW :: MonadWorker m => m (WorkerSite m)
getYesodW = rweSite `liftM` askWorkerEnv

-- | Provides the default `runW` implementation for running SQL queries inside a `Worker`
--defaultRunW :: (PersistConfig c, MonadWorker m, MonadBaseControl IO m) =>
--               (WorkerSite m -> PersistConfigPool c)
--               -> PersistConfigBackend c m b
--               -> m b
defaultRunW connPool f = do
  app <- getYesodW
  runSqlPool f $ connPool app


runWorker :: (Yesod site) => site -> WorkerT site IO a -> IO a
runWorker site worker = runResourceT . withInternalState $ \resState -> do
  logger <- makeLogger site
  let rwe = RunWorkerEnv
            { rweSite = site
            , rweLog = messageLoggerSource site logger
            }
  let wd = WorkerData
           { workerResource = resState
           , workerEnv = rwe
           }
  -- FIXME: catch and handle errors (see unHandlerT)
  unWorkerT worker wd

spawnWorker :: YesodWorker site => site -> Workers -> IO ()
spawnWorker site workers = do
  workerId <- forkIO . forever $ consumeQueue
  runWorker site . $(logInfo) $ "Spawning worker " <> Text.pack (show workerId)
  atomically $ modifyTVar (workerPool workers) (Set.insert workerId)
  return ()

  where
    consumeQueue = do
      mj <- dequeueJob . workerJobQueue $ workers
      case mj of
        Just job -> runWorker site $ perform $ read job
        Nothing  -> threadDelay 1000000

-- TODO: have this accept a ThreadId (from a user post)
killWorker :: YesodWorker site => site -> Workers -> IO ()
killWorker site workers = do
  pool <- atomically . readTVar . workerPool $ workers
  case Set.toList pool of
    w : _ -> do
      runWorker site . $(logInfo) $ "Killing worker " <> Text.pack (show w)
      liftIO $ killThread w
      atomically $ modifyTVar (workerPool workers) (Set.delete w)
    _ -> return ()

-- | Spawns workers which will consume from the application queue
-- performing jobs as they are popped.
startWorkers :: YesodWorker site => site -> [Job] -> IO Workers
startWorkers site jobs = do
  q <- emptyQueue
  pool <- atomically $ newTVar Set.empty
  let workers = Workers { workerJobQueue = q
                        , workerPool = pool
                        }
  replicateM_ (workerCount site) $ spawnWorker site workers
  mapM_ (enqueueJob . workerJobQueue $ workers) jobs
  return workers

-- | Add a job to the site queue from within a Handler
enqueue :: (YesodWorker site) => Job -> HandlerT site IO ()
enqueue job = do
  app <- getYesod
  let q = workerJobQueue . workerSite $ app
  liftIO $ enqueueJob q $ show job
