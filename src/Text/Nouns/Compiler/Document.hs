module Text.Nouns.Compiler.Document where

type Coordinate = Length

data Length = Length Double deriving (Eq)

instance Show Length where
  show (Length x) = show x

data Document = Document [Element] deriving (Show, Eq)

data Element = Rectangle Coordinate Coordinate Length Length deriving (Show, Eq)
