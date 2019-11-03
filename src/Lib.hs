module Lib
    ( generate
    ) where

import Input

type Name = String

type Description = String

data MetricType
    = Counter
    | Gauge
    | Summary
    | Untyped
    | Histogram
    deriving (Show, Eq)

parseMetricType :: String -> MetricType
parseMetricType metricType =
    case metricType of
    ("counter") -> Counter
    ("gauge") -> Gauge
    ("histogram") -> Histogram
    ("summary") -> Summary
    _ -> Untyped

renderMetricType :: MetricType -> String
renderMetricType metricType =
    case metricType of
    Counter -> "fas fa-sort-numeric-up"
    Gauge -> "fas fa-tachometer-alt"
    Histogram -> "fas fa-chart-bar"
    Summary -> "fas fa-file-alt"
    Untyped -> "fas fa-circle"

data ScrapeLine
    = HelpLine String
                String
    | TypeLine String
                MetricType
    | Unknown
    deriving (Show, Eq)

data Metric =
    Metric Name
            Description
            MetricType
    deriving (Show, Eq)

parseMessage :: String -> ScrapeLine
parseMessage line =
    case words line of
    ("#":"HELP":name:description) -> HelpLine name (unwords description)
    ("#":"TYPE":name:metricType) ->
        TypeLine name (parseMetricType (unwords metricType))
    _ -> Unknown

parse :: String -> [ScrapeLine]
parse = map parseMessage . lines

isNotUnknown :: ScrapeLine -> Bool
isNotUnknown (Unknown) = False
isNotUnknown _ = True

skipUnknown :: [ScrapeLine] -> [ScrapeLine]
skipUnknown xs = filter isNotUnknown xs

toPairs :: [ScrapeLine] -> [(ScrapeLine, ScrapeLine)]
toPairs [] = []
toPairs (helpLine:typeLine:restOfList) =
    (helpLine, typeLine) : toPairs restOfList

toMetric :: (ScrapeLine, ScrapeLine) -> Metric
toMetric (x) =
    case x of
    (HelpLine name description, TypeLine _ metricType) ->
        Metric name description metricType

toHtml :: Metric -> String
toHtml (x) =
    case x of
    Metric name description metricType ->
        "\n\
    \            <tr>\n\
    \              <td><i class=\"" ++
        (renderMetricType metricType) ++
        "\"></i></td>\n\
    \              <td><code class=\"highlighter-rouge\">" ++
        name ++
        "</code></td>\n\
    \              <td>" ++
        description ++
        "</td>\n\
    \            </tr>"

toMetrics :: [(ScrapeLine, ScrapeLine)] -> [Metric]
toMetrics (xs) = map toMetric xs

header =
    "\
\<html>\n\
\  <head>\n\
\    <link rel=\"stylesheet\" href=\"./css/style.css\"/>\n\
\    <link rel=\"stylesheet\" href=\"https://use.fontawesome.com/releases/v5.7.2/css/all.css\" integrity=\"sha384-fnmOCqbTlWIlj8LyTjo7mOUStjsKC4pOpQbqyi7RrhN7udi9RwhKkMHpvLbHG9Sr\" crossorigin=\"anonymous\">\n\
\  </head>\n\
\  <body>\n"

tableHeader =
    "\
\    <table>\n\
\      <thead>\n\
\        <tr>\n\
\          <th></th>\n\
\          <th>Name</th>\n\
\          <th>Description</th>\n\
\        </tr>\n\
\      </thead>\n\
\      <tbody>"

tableFooter =
    "\
\      </tbody>\n\
\    </table>\n"

footer =
    "\
\  </body>\n\
\</html>\n"

toDocument :: [Metric] -> String
toDocument (x) = tableHeader ++ (unwords (map toHtml x)) ++ tableFooter

renderFile :: String -> String
renderFile fp = "<h1>" ++ fp ++ "</h1>"

getHeaders :: [String] -> [String]
getHeaders (xs) = fmap renderFile xs

merge :: [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

getFile :: String -> IO String
getFile fp = readFile fp

getOutput :: Maybe FilePath -> String -> IO ()
getOutput (Nothing) = putStrLn
getOutput (Just f)  = writeFile f

extractPaths :: [LogFile] -> [String]
extractPaths a = fmap path a
 
generate :: Maybe FilePath -> String -> IO ()
generate output configuration = do
    
    let config = readInput configuration :: IO [LogFile]
    logFiles <- fmap extractPaths config
    files <- mapM getFile logFiles
        
    let logs :: [String]
        logs = fmap (toDocument . toMetrics . toPairs . skipUnknown . parse) files
        logsWithHeaders :: [String]
        logsWithHeaders = merge (getHeaders logFiles) logs
        
        mergedLog :: String
        mergedLog = header ++ (foldr (++) [] logsWithHeaders) ++ footer
    getOutput output mergedLog
