module Text.Nouns.Compiler.Document
( Number
, Coordinate
, Length
, IRI
, Paint(..)
, Color(..)
, Gradient(..)
, Document(..)
, Element(..)
, black
, showFuncIRI
, localIRIForId
) where

import qualified Network.URI as URI

type Number = Double

type Angle = Number

type Length = Number

type Coordinate = Length

type IRI = URI.URI

showFuncIRI :: IRI -> String
showFuncIRI iri = "url(" ++ show iri ++ ")"

localIRIForId :: String -> IRI
localIRIForId id' = URI.nullURI { URI.uriFragment = fragment }
  where fragment = '#' : URI.escapeURIString URI.isUnescapedInURIComponent id'

newtype Color = Color { hexString :: String } deriving (Eq)

instance Show Color where
  show color = '#' : hexString color

data Gradient = LinearGradient Angle Color Color deriving (Eq, Show)

data Paint = ColorPaint Color
           | GradientPaint Gradient
           deriving (Show, Eq)

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
