module Lib
  ( generate
  )
where

import           Data.Either                (rights)
import           Data.Functor
import           Data.List                  (nub)
import           Data.Map                   as M (Map, empty, findWithDefault,
                                                  insert, toList)
import           Data.Number.Transfinite    ()
import           Data.Scientific            (Scientific)
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

type Name = String

type Description = String

data MetricType
    = Counter
    | Gauge
    | Summary
    | Untyped
    | Histogram
    deriving (Show, Eq, Ord)

data MetricInfo = MetricInfo MetricType String [String]

data CommentLine
    = HelpLine String String
    | TypeLine String MetricType
    | Comment String
    deriving (Show, Eq, Ord)

data Sample = Sample String (Maybe [Label]) String (Maybe Integer) deriving (Show, Eq, Ord)

data Line = CL CommentLine | SL Sample | Blank deriving (Show, Eq, Ord)

data Label = Label { labelName:: String, labelValue:: String} deriving (Show, Eq, Ord)

data Metric = Metric Name Description MetricType deriving (Show, Eq, Ord)

newtype Metrics = Metrics [Line] deriving (Show, Eq, Ord)

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

renderMetricType :: MetricType -> String
renderMetricType metricType = case metricType of
  Counter   -> "fas fa-sort-numeric-up"
  Gauge     -> "fas fa-tachometer-alt"
  Histogram -> "fas fa-chart-bar"
  Summary   -> "fas fa-file-alt"
  Untyped   -> "fas fa-circle"

helpLineP :: Parser CommentLine
helpLineP = do
  char '#'
  space
  string "HELP"
  space
  metric <- identifier
  space
  help <-  manyTill anySingle eol
  pure $ HelpLine metric help

typeLineP :: Parser CommentLine
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

commentP :: Parser CommentLine
commentP = do
  char '#'
  space
  comment <- manyTill anySingle eol
  pure $ Comment comment

commentLineP :: Parser Line
commentLineP = CL <$> (try typeLineP <|> try helpLineP <|> commentP)

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
  pure $ SL $ Sample metric labels value timestamp

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

extractPaths :: [LogFile] -> [String]
extractPaths = fmap path

getFile :: String -> IO String
getFile = readFile

generate :: Maybe FilePath -> String -> String -> IO ()
generate output configuration css = do
  let config = readInput configuration :: IO [LogFile]
  logFiles <- fmap extractPaths config
  files    <- traverse getFile logFiles
  let parsed = parse metricsP "" <$> files
  let rendered = (fmap . fmap) (toDocument . sortMetrics) parsed
  let logsWithHeaders = merge (getHeaders logFiles) (rights rendered)
  let mergedLog = header css ++ concat logsWithHeaders ++ footer
  getOutput output mergedLog

newMetricInfo :: MetricInfo
newMetricInfo = MetricInfo Untyped "" []

sortMetrics :: Metrics -> Map String MetricInfo
sortMetrics (Metrics lines) = Prelude.foldr go M.empty lines
  where
    go :: Line -> Map String MetricInfo -> Map String MetricInfo
    go metric acc = case metric of
      (CL (HelpLine m help)) -> M.insert m (setHelp (findWithDefault newMetricInfo m acc) help) acc
      (CL (TypeLine m t)) -> M.insert m (setType (findWithDefault newMetricInfo m acc) t) acc
      (SL (Sample m (Just labels) _value _)) -> M.insert m (setLabels (findWithDefault newMetricInfo m acc) labels) acc
      _ -> acc

setHelp :: MetricInfo -> String -> MetricInfo
setHelp (MetricInfo t _ labels) help = MetricInfo t help labels

setType :: MetricInfo -> MetricType -> MetricInfo
setType (MetricInfo _ help labels) t = MetricInfo t help labels

setLabels :: MetricInfo -> [Label] -> MetricInfo
setLabels (MetricInfo t help labels) l = MetricInfo t help $ nub $ labels ++ fmap labelName l

toHtml :: String -> MetricInfo -> String
toHtml name x = case x of
  MetricInfo t description lables ->
    "\n\
    \            <tr>\n\
    \              <td><i class=\""
      ++ renderMetricType t
      ++ "\"></i></td>\n\
    \              <td><code class=\"highlighter-rouge\">"
      ++ name
      ++ "</code></td>\n\
    \              <td>"
      ++ description
      ++ "</td>\n\
    \            </tr>"

header :: String -> String
header css =
  "\
\<html>\n\
\  <head>\n\
\    <link rel=\"stylesheet\" href=\""
    ++ css
    ++ "\"/>\n\
\    <link rel=\"stylesheet\" href=\"https://use.fontawesome.com/releases/v5.7.2/css/all.css\" integrity=\"sha384-fnmOCqbTlWIlj8LyTjo7mOUStjsKC4pOpQbqyi7RrhN7udi9RwhKkMHpvLbHG9Sr\" crossorigin=\"anonymous\">\n\
\  </head>\n\
\  <body>\n"

tableHeader
  = "\
\    <table>\n\
\      <thead>\n\
\        <tr>\n\
\          <th></th>\n\
\          <th>Name</th>\n\
\          <th>Description</th>\n\
\        </tr>\n\
\      </thead>\n\
\      <tbody>"

tableFooter = "\
\      </tbody>\n\
\    </table>\n"

footer = "\
\  </body>\n\
\</html>\n"

toDocument :: Map String MetricInfo -> String
toDocument x = tableHeader ++ unwords (fmap (\y -> toHtml (fst y) (snd y)) (toList x)) ++ tableFooter

renderFile :: String -> String
renderFile fp = "<h1>" ++ fp ++ "</h1>"

getHeaders :: [String] -> [String]
getHeaders = fmap renderFile

merge :: [a] -> [a] -> [a]
merge xs       []       = xs
merge []       ys       = ys
merge (x : xs) (y : ys) = x : y : merge xs ys

getOutput :: Maybe FilePath -> String -> IO ()
getOutput Nothing  = putStrLn
getOutput (Just f) = writeFile f
