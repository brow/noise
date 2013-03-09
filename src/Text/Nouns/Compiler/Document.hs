module Text.Nouns.Compiler.Document
( Coordinate
, Length
, Color(..)
, black
, Document(..)
, Element(..)
) where

type Coordinate = Length

type Length = Double

newtype Color = Color { hexString :: String } deriving (Show, Eq)

black :: Color
black = Color "000000"

data Document = Document [Element] deriving (Show, Eq)

data Element = Rectangle { x :: Coordinate
                         , y :: Coordinate
                         , width :: Length
                         , height :: Length
                         , cornerRadius :: Length
                         , fill :: Color
                         }
             | Circle { cx :: Coordinate
                      , cy :: Coordinate
                      , r :: Length
                      , fill :: Color
                      }
             deriving (Show, Eq)
