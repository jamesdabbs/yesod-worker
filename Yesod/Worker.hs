module Yesod.Worker
( defaultRunW
, enqueue
, emptyQueue
, JobQueue
, spawnWorkers
, WorkerT
, YesodWorker (..)
) where

import Prelude

import Yesod
import Yesod.Worker.Queue
import Yesod.Worker.Types

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, liftM, replicateM_)
import Control.Monad.Trans.Resource (runResourceT, withInternalState)


askWorkerEnv :: MonadWorker m => m (RunWorkerEnv (WorkerSite m))
askWorkerEnv = liftWorkerT $ WorkerT $ return . workerEnv

getYesodW :: MonadWorker m => m (WorkerSite m)
getYesodW = rweSite `liftM` askWorkerEnv

defaultRunW :: (PersistConfig c, MonadWorker m, MonadBaseControl IO m) =>
               (WorkerSite m -> c)
               -> (WorkerSite m -> PersistConfigPool c)
               -> PersistConfigBackend c m b
               -> m b
defaultRunW persistConfig connPool f = do
  app <- getYesodW
  runPool (persistConfig app) f (connPool app)


runWorker :: (Yesod site) => site -> (WorkerT site IO a) -> IO a
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

spawnWorkers :: YesodWorker site => site -> IO ()
spawnWorkers site = replicateM_ (workerCount site) . forkIO . forever $ do
  mj <- dequeueJob $ queue site
  case mj of
    Just job -> runWorker site $ perform job
    Nothing -> threadDelay 1000000

enqueue :: YesodWorker site => Job -> HandlerT site IO ()
enqueue job = do
  app <- getYesod
  liftIO $ enqueueJob (queue app) job
