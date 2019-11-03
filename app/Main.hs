module Main where

import Lib

import Options.Applicative

data Cmdline = Cmdline
  { output        :: Maybe String
  , configuration :: String
  , verbose       :: Bool }

cmdline :: Parser Cmdline
cmdline = Cmdline
    <$> optional (strOption
        ( long "output"
        <> short 'o'
        <> metavar "OUTPUT"
        <> help "Write to a file instead of stdout" ))
    <*> strOption
        ( long "config"
        <> short 'c'
        <> metavar "CONFIG"
        <> value "input.yaml"
        <> help "Specify a configuration file (default is input.yaml)" )
    <*> switch
        ( long "verbose"
        <> short 'v'
        <> help "Make the operation more talkative" )

main :: IO ()
main = promDocs =<< execParser opts
  where
    opts = info (cmdline <**> helper)
      ( fullDesc
     <> progDesc "Prometheus metrics reference generator"
     <> header "prom-docs - a metrics reference generator for Prometheus" )

promDocs :: Cmdline -> IO ()
promDocs (Cmdline output configuration _) = generate output configuration
