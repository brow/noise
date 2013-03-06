module Text.Nouns.Compiler.Document
( Coordinate
, Length
, Document(..)
, Element(..)
) where

type Coordinate = Length

type Length = Double

data Document = Document [Element] deriving (Show, Eq)

data Element = Rectangle { x :: Coordinate
                         , y :: Coordinate
                         , width :: Length
                         , height :: Length
                         , cornerRadius :: Length
                         }
             | Circle { cx :: Coordinate
                      , cy :: Coordinate
                      , r :: Length
                      }
             deriving (Show, Eq)
