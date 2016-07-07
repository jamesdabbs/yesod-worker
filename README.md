A Sidekiq-compatible background worker system, built on [Keenser](https://github.com/jamesdabbs/keenser) and using Redis for job persistence

# Usage

See [the demo](https://github.com/jamesdabbs/yesod-worker-demo) for an example setup

## Define Your Jobs

In e.g. `Jobs.hs`

```
import Yesod.Worker

blah :: Worker Handler (Text, Int)
-- Worker name (should be unique), queue name, then arg list
blah = Worker "blah" "default" $ \(word, n) ->
  -- Your long-running work goes here
  -- N.B. this runs in the Yesod Handler monad, but doesn't have access to
  --   anything request- or response- related. Under the hood, it's using
  --   `unsafeHandler`, and may error if you try to e.g. redirect. A future
  --   version will include a `Handler`-like monad stack which should be
  --   safe from these sorts of errors.
  replicateM_ n $ do
    $(logWarn) word
    liftIO . threadDelay $ 1000000

setup = do
  register blah
  -- register any other jobs you like
  -- they need not have the same argument type
```

## Include the Subsite

In `Foundation.hs`

```
import Yesod.Worker

data App = App
  { ...
  , getWorkers :: Workers
  }

instance YesodWorker App where
  workers = getWorkers
```

In `Application.hs`

```
import Yesod.Worker
import Jobs

makeFoundation = do
  ...
  getWorkers <- newWorkers
  ...

makeApplication foundation = do
  unsafeHandler foundation $ do
    bootWorkers setup
    -- optionally, queue jobs on app boot:
    enqueue blah ("queued at start", 2)
```

## Queue Up a Job

In e.g. `Handlers/Whatever.hs`

```
import Yesod.Worker
import Jobs

postHandlerR = do 
  enqueue blah ("na", 16)
  ...
```

## Watch the Stats

Add a route

```
/workers WorkerR Workers getWorkers
```

and visit `/workers`

Over time, this page should expand to a Sidekiq-style administration panel, allowing for closer monitoring and options for retrying from the web UI.

Note that the Redis interface is already Sidekiq compatable, so you can literally use `sidekiq/web` for monitoring as well.
