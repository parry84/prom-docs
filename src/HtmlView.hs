{-# LANGUAGE OverloadedStrings #-}

module HtmlView
  ( producePage
  )
where

import           Data.Either                     (fromRight, isRight, rights)
import           Data.Functor
import           Data.List                       (nub)
import           Data.Map                        as M (Map, empty,
                                                       findWithDefault, insert,
                                                       toList)
import           Types

import           Text.Blaze.Html.Renderer.Pretty (renderHtml)
import           Text.Blaze.Html5                as H (Html, code, h1, head,
                                                       html, i, link,
                                                       stringValue, table, td,
                                                       th, thead, title, toHtml,
                                                       tr, (!))
import           Text.Blaze.Html5.Attributes     as A (class_, href, rel)

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
      H.thead $
        H.tr $ do
          H.th ""
          H.th "Name"
          H.th "Description"
      mconcat $ fmap renderRow $ toList $ snd file

renderRow :: (String, MetricInfo) -> Html
renderRow (name, MetricInfo t description lables) = do
   H.tr $ do
      H.td $
        H.i ! class_ (stringValue ( renderMetricType t )) $ do ""
      H.td $
        H.code ! class_ "highlighter-rouge" $ do toHtml name
      H.td $
        H.code $ do toHtml description

renderMetricType :: MetricType -> String
renderMetricType metricType = case metricType of
  Counter   -> "fas fa-sort-numeric-up"
  Gauge     -> "fas fa-tachometer-alt"
  Histogram -> "fas fa-chart-bar"
  Summary   -> "fas fa-file-alt"
  Untyped   -> "fas fa-circle"
