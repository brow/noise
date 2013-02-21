module Text.Nouns.Compiler.Document
( Coordinate
, Length(..)
, Document(..)
, Element(..)
) where

type Coordinate = Length

data Length = Length Double deriving (Eq)

instance Show Length where
  show (Length a) = show a

data Document = Document [Element] deriving (Show, Eq)

data Element = Rectangle { x :: Coordinate
                         , y :: Coordinate
                         , width :: Length
                         , height :: Length
                         }
             deriving (Show, Eq)
