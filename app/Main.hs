{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import qualified Control.Exception    as E
import           Control.Monad.Except (ExceptT, MonadError (throwError),
                                       MonadIO (..), runExceptT)
import           Control.Monad.Reader (MonadIO (..), MonadReader, ReaderT (..),
                                       asks)
import qualified Data.Bifunctor       as BF
import qualified Data.Bool            as B
import qualified Data.Char            as C
import           Data.Map
import           HtmlView             (producePage)
import           Input                (LogFile (path), readInput)
import           Options.Applicative
import           Parser               (toMetricInfo)
import           Types

type FileName = String

data Options = Options
    { oOutput        :: Maybe FileName
    , oConfiguration :: FileName
    , oCss           :: FileName
    , oVerbose       :: Bool
    }

type AppConfig = MonadReader Options

newtype AppError = IOError E.IOException

newtype App a = App {
    runApp :: ReaderT Options (ExceptT AppError IO) a
} deriving (Monad, Functor, Applicative, AppConfig, MonadIO, MonadError AppError)

cmdline :: Parser Options
cmdline =
  Options
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
main = runProgram =<< execParser opts
 where
  opts = info
    (cmdline <**> helper)
    (fullDesc <> progDesc "Prometheus metrics reference generator" <> header
      "prom-docs - a metrics reference generator for Prometheus"
    )

runProgram :: Options -> IO ()
runProgram o = either renderError return =<< runExceptT (runReaderT (runApp run) o)

renderError :: AppError -> IO ()
renderError (IOError e) = do
    putStrLn "There was an error:"
    putStrLn $ "  " ++ show e

run :: App ()
run = write =<< render =<< parse =<< getSource

write :: String -> App ()
write h = liftIO . maybe (putStrLn h) (`writeFile` h) =<< asks oOutput

render :: AppConfig m => [(String, Map String MetricInfo)] -> m String
render ms = asks ((`producePage` ms) . oCss)

parse ::[LogFile] -> App [(String, Map String MetricInfo)]
parse x = do liftIO $ toMetricInfo x

getSource :: App [LogFile]
getSource = do readInput <$> loadContents

loadContents :: App String
loadContents =
    readFileFromOptions =<< asks oConfiguration
  where
    readFileFromOptions f = either throwError return . BF.first IOError =<< liftIO (safeReadFile f)

safeReadFile :: FilePath -> IO (Either E.IOException String)
safeReadFile = E.try . readFile
