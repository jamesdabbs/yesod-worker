{-# LANGUAGE RecordWildCards #-}
module Yesod.Worker.Util
  ( enqueue
  , enqueueAt
  , enqueueIn
  ) where

import qualified Keenser as K
import Yesod
import Yesod.Worker.Types

import Control.Concurrent (readMVar)
import Data.Thyme.Clock   (UTCTime)

enqueue :: (ToJSON a, YesodWorker master) => K.Worker (HandlerT master IO) a -> a -> HandlerT master IO ()
enqueue job args = do
  manager <- getManager
  K.enqueue manager job args

enqueueAt :: (ToJSON a, YesodWorker master) => UTCTime -> K.Worker (HandlerT master IO) a -> a -> HandlerT master IO ()
enqueueAt time job args = do
  manager <- getManager
  K.enqueueAt time manager job args

enqueueIn :: (ToJSON a, YesodWorker master) => Rational -> K.Worker (HandlerT master IO) a -> a -> HandlerT master IO ()
enqueueIn snds job args = do
  manager <- getManager
  K.enqueueIn snds manager job args

getManager :: YesodWorker master => HandlerT master IO K.Manager
getManager = do
  Workers{..} <- workers <$> getYesod
  liftIO $ do
    m <- readMVar wManager
    return $! m
