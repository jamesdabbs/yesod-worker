{-# LANGUAGE ConstraintKinds, FlexibleInstances, UndecidableInstances, ViewPatterns #-}
module Yesod.Worker.Types where

import Prelude
import Yesod

import Control.Applicative (Applicative (..))
import Control.Concurrent (ThreadId)
import Control.Concurrent.STM (TVar)
import Control.Monad (liftM, ap)
import Control.Monad.Logger (LogSource, MonadLogger (..))
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Control.Monad.Trans.Resource (InternalState, runInternalState, MonadThrow (..), monadThrow, MonadResourceBase)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Database.Persist.Sql (SqlPersistT)
import Language.Haskell.TH.Syntax (Loc)
import System.Log.FastLogger (LogStr, toLogStr)


type JobQueue = TVar (Seq.Seq String)
type WorkerPool = TVar (Set.Set ThreadId)

-- | Foundation type for the Worker subsite
data Workers = Workers
  { workerJobQueue :: JobQueue
  , workerPool :: WorkerPool
  }

mkYesodSubData "Workers" $(parseRoutesFile "routes")

-- | Typeclass for customizing Worker settings
class (Yesod site, Read Job, Show Job) => YesodWorker site where
  -- | Your application's job type
  type Job

  workerSite :: site -> Workers

  -- | Number of concurrent workers
  workerCount :: site -> Int
  workerCount _ = 3

  -- | How to execute each job
  perform :: Job -> WorkerT site IO ()

  -- TODO: Allow different PersistBackends
  -- | Helper for running SQL queries inside a Worker
  runW :: SqlPersistT (WorkerT site IO) a -> WorkerT site IO a


data RunWorkerEnv site = RunWorkerEnv
  { rweSite     :: !site
  , rweLog      :: !(Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -- , rheOnError  :: !(ErrorResponse -> YesodApp)
  }

data WorkerData site = WorkerData
  { workerResource :: !InternalState
  , workerEnv      :: !(RunWorkerEnv site)
  }

newtype WorkerT site m a = WorkerT
  { unWorkerT :: WorkerData site -> m a
  }


instance MonadTrans (WorkerT site) where
  lift = WorkerT . const

instance Monad m => Functor (WorkerT site m) where
  fmap = liftM

instance Monad m => Applicative (WorkerT site m) where
  pure = return
  (<*>) = ap

instance MonadIO m => MonadIO (WorkerT site m) where
  liftIO = lift . liftIO

instance MonadBase b m => MonadBase b (WorkerT site m) where
  liftBase = lift . liftBase

-- TODO: absorb the instance declarations below
instance Monad m => Monad (WorkerT site m) where
  return = WorkerT . const . return
  WorkerT x >>= f = WorkerT $ \r -> x r >>= \x' -> unWorkerT (f x') r

instance MonadBaseControl b m => MonadBaseControl b (WorkerT site m) where
  data StM (WorkerT site m) a = StH (StM m a)
  liftBaseWith f = WorkerT $ \reader ->
    liftBaseWith $ \runInBase ->
      f $ liftM StH . runInBase . (\(WorkerT r) -> r reader)
  restoreM (StH base) = WorkerT $ const $ restoreM base

instance MonadThrow m => MonadThrow (WorkerT site m) where
  throwM = lift . monadThrow

instance (MonadIO m, MonadBase IO m, MonadThrow m) => MonadResource (WorkerT site m) where
  liftResourceT f = WorkerT $ \hd -> liftIO $ runInternalState f (workerResource hd)

instance MonadIO m => MonadLogger (WorkerT site m) where
  monadLoggerLog a b c d = WorkerT $ \hd ->
    liftIO $ rweLog (workerEnv hd) a b c (toLogStr d)

class MonadResource m => MonadWorker m where
  type WorkerSite m
  liftWorkerT :: WorkerT (WorkerSite m) IO a -> m a

instance MonadResourceBase m => MonadWorker (WorkerT site m) where
  type WorkerSite (WorkerT site m) = site
  liftWorkerT (WorkerT f) = WorkerT $ liftIO . f
{-# RULES "liftWorkerT (WorkerT site IO)" liftWorkerT = id #-}
