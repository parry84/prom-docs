module Main where

import Lib

import Options.Applicative

data Cmdline = Cmdline
  { file       :: Maybe String
  , verbose    :: Bool }

cmdline :: Parser Cmdline
cmdline = Cmdline
    <$> optional (strOption
        ( long "file"
        <> short 'f'
        <> metavar "FILE"
        <> help "Write to FILE instead of stdout" ))
    <*> switch
        ( long "verbose"
        <> short 'v'
        <> help "Make the operation more talkative" )

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
main = promDocs =<< execParser opts
  where
    opts = info (cmdline <**> helper)
      ( fullDesc
     <> progDesc "Prometheus metrics reference generator"
     <> header "prom-docs - a  metrics reference generator for Prometheus" )

promDocs :: Cmdline -> IO ()
promDocs (Cmdline output _) = generate logFiles output
