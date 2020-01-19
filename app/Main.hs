module Main where

import           Lib

import           Options.Applicative

type FileName = String

data Cmdline = Cmdline
  { output        :: Maybe FileName
  , configuration :: FileName
  , css           :: FileName
  , verbose       :: Bool }

cmdline :: Parser Cmdline
cmdline =
  Cmdline
    <$> optional
          (strOption
            (long "output" <> short 'o' <> metavar "OUTPUT" <> help
              "Write to a file instead of stdout"
            )
          )
    <*> strOption
          (  long "config"
          <> short 'c'
          <> metavar "CONFIG"
          <> value "input.yaml"
          <> help "Specify a configuration file (default is input.yaml)"
          )
    <*> strOption
          (  long "css"
          <> short 's'
          <> metavar "CSS"
          <> value "./css/style.css"
          <> help "Specify a CSS file (default is ./css/style.css)"
          )
    <*> switch
          (long "verbose" <> short 'v' <> help
            "Make the operation more talkative"
          )

main :: IO ()
main = promDocs =<< execParser opts
 where
  opts = info
    (cmdline <**> helper)
    (fullDesc <> progDesc "Prometheus metrics reference generator" <> header
      "prom-docs - a metrics reference generator for Prometheus"
    )

promDocs :: Cmdline -> IO ()
promDocs (Cmdline output configuration css _) =
  generate output configuration css
