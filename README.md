See [the demo](https://github.com/jamesdabbs/yesod-worker-demo) for an example site with workers set up.

# Usage

These instructions are for v0.0.1. The next version is in progress, but will feature a Worker subsite and new installation instructions.

## Define your Job type

```
-- Up to you where this goes, so long as it's importable
data AppJob = CountJob Int | UserJob
```

## Include a queue in your foundation data type

```
-- Foundation.hs
import Yesod.Worker (JobQueue, YesodWorker(..), defaultRunW)

data App = App
  { settings :: AppConfig DefaultEnv Extra
  ...
  , appQueue :: JobQueue AppJob
  }
```

## Specify how to run your jobs
```
-- Foundation.hs (provided you don't want to orphan the instance)
instance YesodWorker App where
  type Job = AppJob
  queue = appQueue
  runW = defaultRunW persistConfig connPool

  -- Dummy implementations, obviously these would depend on your app
  perform (CountJob n) = void . forM [1..n] $ \k -> do
    lift . putStrLn . show $ k
    lift $ threadDelay 1000000
  perform UserJob = do
    -- Note that you can run SQL using the `runW` helper
    n <- runW $ count ([] :: [Filter User])
    lift . putStrLn $ "There are " ++ (show n) ++ " users"
```

## Start the queue on app boot
```
-- Application.hs
import Yesod.Worker (emptyQueue, spawnWorkers)

makeFoundation conf = do
  ...
  q <- emptyQueue
  let foundation = App conf ... q
  spawnWorkers foundation
  ...
```

## Queue up a job
```
import Yesod.Worker (enqueue)

getWhateverR = do
  enqueue $ CountJob 10
  ...
```

# TODO

* Simplify installation?
* Include job subsite for viewing queue and worker status
* Swappable queue backends (for e.g. Redis)
* Improve error handling / job failures / worker restarts
* Allow multiple queues with customizable priority
