module Main where

import Lib

import Options.Applicative

-- | Files where the logs are stored.
--   Modify this value to read logs from
--   other sources.
logFiles :: [Lib.File]
logFiles =
    [ Local "os.metrics"
    , Local "api.metrics"
    , Local "engine.metrics"
    , Local "scheduler.metrics"
    , Local "queue.metrics"
    , Local "db.metrics"
    ]


main :: IO ()
main = generate logFiles
