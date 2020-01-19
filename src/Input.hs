{-# LANGUAGE OverloadedStrings #-}

module Input
  ( readInput
  , File(..)
  , LogFile(..)
  )
where

import qualified Data.ByteString.Char8         as BS
import           Data.Maybe                     ( fromJust )
import           Data.Yaml

data File
  = URL String
  | Local FilePath

data LogFile = LogFile
  { source :: String
  , path :: String
  } deriving (Show)

instance FromJSON LogFile where
  parseJSON (Object m) = LogFile <$> m .: "source" <*> m .: "path"
  parseJSON _ = error "Can't parse LogFile from YAML/JSON"

readInput :: String -> IO [LogFile]
readInput f = do
  ymlData <- BS.readFile f
  let logFiles = Data.Yaml.decode ymlData :: Maybe [LogFile]
  return $ fromJust logFiles
