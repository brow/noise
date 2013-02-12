module Text.Nouns.Compiler.Document where

type Coordinate = Length

data Length = Length Double

instance Show Length where
  show (Length x) = show x

data Document = Document [Element] deriving Show

data Element = Rectangle Coordinate Coordinate Length Length deriving Show
