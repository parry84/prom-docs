{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types
  ( MetricType(..),
    MetricInfo(..),
    Metrics(..),
    Line(..),
    Label(..),
  )
where

type Name = String

type Description = String

type Value = String

data MetricType
  = Counter
  | Gauge
  | Summary
  | Untyped
  | Histogram
  deriving (Show, Eq, Ord)

data MetricInfo = MetricInfo MetricType Name [Description]

data Line
  = HelpLine Name Description
  | TypeLine Name MetricType
  | Comment String
  | Sample Name (Maybe [Label]) Value (Maybe Integer)
  | Blank
  deriving (Show, Eq, Ord)

data Label = Label { labelName:: String, labelValue:: String} deriving (Show, Eq, Ord)

data Metric = Metric Name Description MetricType deriving (Show, Eq, Ord)

newtype Metrics = Metrics [Line] deriving (Show, Eq, Ord)
