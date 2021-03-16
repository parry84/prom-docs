module Parser
  ( toMetricInfo
  )
where

import           Data.Either                (rights)
import           Data.Functor
import           Data.List                  (nub)
import           Data.Map                   as M (Map, empty, findWithDefault,
                                                  insert, toList)
import           Input                      (LogFile (path), readInput)
import           Text.Megaparsec            (MonadParsec (try), Parsec,
                                             anySingle, many, manyTill, noneOf,
                                             oneOf, optional, parse, sepBy,
                                             some, (<|>))
import           Text.Megaparsec.Char       (alphaNumChar, char, eol,
                                             letterChar, space, spaceChar,
                                             string)
import           Text.Megaparsec.Char.Lexer (decimal, float, lexeme, scientific,
                                             signed)
import           Types


type Parser = Parsec Metrics String

metricTypeP :: Parser MetricType
metricTypeP = string "counter" $>  Counter
       <|> string "gauge" $>  Gauge
       <|> string "histogram" $>  Histogram
       <|> string "summary" $> Summary

parseMetricType :: String -> MetricType
parseMetricType metricType = case metricType of
  "counter"   -> Counter
  "gauge"     -> Gauge
  "histogram" -> Histogram
  "summary"   -> Summary
  _           -> Untyped

helpLineP :: Parser Line
helpLineP = do
  char '#'
  space
  string "HELP"
  space
  metric <- identifier
  space
  help <-  manyTill anySingle eol
  pure $ HelpLine metric help

typeLineP :: Parser Line
typeLineP = do
  char '#'
  space
  string "TYPE"
  space
  metric <- identifier
  space
  t <- metricTypeP
  eol
  pure $ TypeLine metric t

commentP :: Parser Line
commentP = do
  char '#'
  space
  comment <- manyTill anySingle eol
  pure $ Comment comment

commentLineP :: Parser Line
commentLineP = try typeLineP <|> try helpLineP <|> commentP

timestampP :: Parser Integer
timestampP = do
  some $ char ' '
  signed space decimal

sampleP :: Parser Line
sampleP = do
  metric <- identifier
  labels <- optional labelsP
  space
  value <-  show <$> scientific <|> string "+Inf" <|> string "-Inf" <|> string "NaN"
  timestamp <- optional timestampP
  eol
  pure $ Sample metric labels value timestamp

labelsP :: Parser [Label]
labelsP = do
  char '{'
  labels <- labelP `sepBy` char ','
  char '}'
  pure labels

labelP :: Parser Label
labelP = do
  label <- identifier
  char '='
  Label label <$> stringP

escape :: Parser String
escape = do
    d <- char '\\'
    c <- oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
    return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

character :: Parser String
character = fmap return nonEscape <|> escape

stringP :: Parser String
stringP = do
    char '"'
    strings <- many character
    char '"'
    return $ concat strings

identifier :: Parser String
identifier = some (alphaNumChar <|> char '_')


blankP :: Parser Line
blankP = do
  _ <- eol
  pure Blank

lineP :: Parser Line
lineP = try commentLineP <|> sampleP <|> blankP

metricsP :: Parser Metrics
metricsP = Metrics <$> some lineP

toMetricInfo :: [LogFile] -> IO [(String, Map String MetricInfo)]
toMetricInfo config = do
  let logFiles = extractPaths config
  files    <- traverse getFile logFiles
  let parsed = parse metricsP "" <$> files
  pure $ zip logFiles $ rights $ (fmap . fmap) sortMetrics parsed

extractPaths :: [LogFile] -> [String]
extractPaths = fmap path

getFile :: String -> IO String
getFile = readFile


sortMetrics :: Metrics -> Map String MetricInfo
sortMetrics (Metrics lines) = Prelude.foldr go M.empty lines
  where
    go :: Line -> Map String MetricInfo -> Map String MetricInfo
    go metric acc = case metric of
      (HelpLine m help) -> M.insert m (setHelp (findWithDefault newMetricInfo m acc) help) acc
      (TypeLine m t) -> M.insert m (setType (findWithDefault newMetricInfo m acc) t) acc
      (Sample m (Just labels) _value _) -> M.insert m (setLabels (findWithDefault newMetricInfo m acc) labels) acc
      _ -> acc

setHelp :: MetricInfo -> String -> MetricInfo
setHelp (MetricInfo t _ labels) help = MetricInfo t help labels

setType :: MetricInfo -> MetricType -> MetricInfo
setType (MetricInfo _ help labels) t = MetricInfo t help labels

setLabels :: MetricInfo -> [Label] -> MetricInfo
setLabels (MetricInfo t help labels) l = MetricInfo t help $ nub $ labels ++ fmap labelName l

newMetricInfo :: MetricInfo
newMetricInfo = MetricInfo Untyped "" []
