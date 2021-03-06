{-# LANGUAGE OverloadedStrings #-}

module Input
  ( readInput
  , File(..)
  , LogFile(..)
  )
where

import qualified Data.ByteString.Char8 as BS
import           Data.Maybe            (fromJust)
import           Data.Yaml             (FromJSON (parseJSON), Value (Object),
                                        decodeEither', (.:))

data File
  = URL String
  | Local FilePath

data LogFile = LogFile
  { source :: String
  , path   :: String
  } deriving (Show)

instance FromJSON LogFile where
  parseJSON (Object m) = LogFile <$> m .: "source" <*> m .: "path"
  parseJSON _          = error "Can't parse LogFile from YAML/JSON"

readInput :: String -> [LogFile]
readInput ymlData = do
  case Data.Yaml.decodeEither' (BS.pack ymlData) of
    Left err       -> error $ "Could not parse input.yaml: " ++ show err
    Right logFiles -> logFiles

