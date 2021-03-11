module Types
  ( MetricType(..),
    MetricInfo(..),
    Metrics(..),
    Line(..),
    CommentLine(..),
    Sample(..),
    Label(..)
  )
where

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
