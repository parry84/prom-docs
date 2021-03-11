{-# LANGUAGE OverloadedStrings #-}

module HtmlView
  ( generate
  )
where

import           Data.Either                (rights, fromRight, isRight)
import           Data.Functor
import           Data.List                  (nub)
import           Data.Map                   as M (Map, empty, findWithDefault,
                                                  insert, toList)
import           Data.Number.Transfinite    ()
import           Data.Scientific            (Scientific)
import           Input                      (LogFile (path), readInput)
import           Parser                     (metricsP)
import           Text.Megaparsec            (parse)
import           Types

import Text.Blaze.Html5 as H
    ( toHtml,
      code,
      h1,
      head,
      html,
      i,
      link,
      table,
      td,
      th,
      thead,
      title,
      tr,
      stringValue,
      Html,
      (!) )
import Text.Blaze.Html5.Attributes as A ( class_, href, rel )
import Text.Blaze.Html.Renderer.Pretty (renderHtml)

producePage :: String -> [(String, Map String MetricInfo)] -> String
producePage css logFiles = renderHtml $ go css logFiles

go :: String -> [(String, Map String MetricInfo)] -> Html
go css logFiles = html $ do
  H.head $ do
    H.title "Prometheus metrics reference"
    H.link ! rel "stylesheet" ! href (stringValue css)
    H.link ! rel "stylesheet" ! href "https://use.fontawesome.com/releases/v5.7.2/css/all.css"
    mconcat $ fmap renderTable logFiles

renderTable :: (String, Map String MetricInfo) -> Html
renderTable file = do
    H.h1 $ toHtml $ fst file
    H.table $ do
      H.thead $ do
        H.tr $ do
          H.th ""
          H.th "Name"
          H.th "Description"
    mconcat $ fmap renderRow $ toList $ snd file

renderRow :: (String, MetricInfo) -> Html
renderRow (name, MetricInfo t description lables) = do
   H.tr $ do
      H.td $ do
        H.i ! class_ (stringValue ( renderMetricType t )) $ do ""
      H.td $ do
        H.code ! class_ "highlighter-rouge" $ do toHtml name
        H.code $ do toHtml description

renderMetricType :: MetricType -> String
renderMetricType metricType = case metricType of
  Counter   -> "fas fa-sort-numeric-up"
  Gauge     -> "fas fa-tachometer-alt"
  Histogram -> "fas fa-chart-bar"
  Summary   -> "fas fa-file-alt"
  Untyped   -> "fas fa-circle"


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
  let sorted = zip logFiles $ rights $ (fmap . fmap) sortMetrics parsed
  getOutput output $ producePage css sorted

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

getOutput :: Maybe FilePath -> String -> IO ()
getOutput Nothing  = putStrLn
getOutput (Just f) = writeFile f
