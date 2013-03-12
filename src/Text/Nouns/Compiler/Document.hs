module Text.Nouns.Compiler.Document
( Coordinate
, Length
, Paint(..)
, Color(..)
, black
, Document(..)
, Element(..)
) where

type Coordinate = Length

type Length = Double

newtype Color = Color { hexString :: String } deriving (Show, Eq)

data Paint = ColorPaint Color deriving (Show, Eq)

black :: Paint
black = ColorPaint $ Color "000000"

data Document = Document [Element] deriving (Show, Eq)

data Element = Rectangle { x :: Coordinate
                         , y :: Coordinate
                         , width :: Length
                         , height :: Length
                         , cornerRadius :: Length
                         , fill :: Paint
                         }
             | Circle { cx :: Coordinate
                      , cy :: Coordinate
                      , r :: Length
                      , fill :: Paint
                      }
             deriving (Show, Eq)
