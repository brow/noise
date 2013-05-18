module Text.Noise.Compiler.Document
( Number
, Angle
, Coordinate
, Length
, Color
, IRI
, OpacityValue
, Paint(..)
, Gradient(..)
, Document(..)
, Element(..)
, PathCommand(..)
, circle
, rectangle
, path
, defaultValue
, showFuncIRI
, localIRIForId
, fileIRI
) where

import qualified Network.URI as URI
import Text.Noise.Compiler.Document.Color (Color)
import qualified Text.Noise.Compiler.Document.Color as Color

type Number = Double

type Angle = Number

type Length = Number

type Coordinate = Length

type IRI = URI.URI

type OpacityValue = Number

showFuncIRI :: IRI -> String
showFuncIRI iri = "url(" ++ show iri ++ ")"

isUnescapedInURIComponent :: Char -> Bool
isUnescapedInURIComponent c = not (URI.isReserved c || not (URI.isUnescapedInURI c))

localIRIForId :: String -> IRI
localIRIForId id' = URI.nullURI { URI.uriFragment = fragment }
  where fragment = '#' : URI.escapeURIString isUnescapedInURIComponent id'

fileIRI :: String -> Maybe IRI
fileIRI = URI.parseRelativeReference

data Gradient = LinearGradient { angle :: Angle
                               , stops :: [(Number,Color)]
                               }
              | RadialGradient { stops :: [(Number,Color)]
                               }
              deriving (Eq, Show)

data Paint = ColorPaint Color
           | GradientPaint Gradient
           deriving (Show, Eq)

data Document = Document [Element] deriving (Show, Eq)

data Element = Rectangle { x :: Coordinate
                         , y :: Coordinate
                         , width :: Length
                         , height :: Length
                         , cornerRadius :: Length
                         , fill :: Paint
                         , stroke :: Paint
                         }
             | Circle { cx :: Coordinate
                      , cy :: Coordinate
                      , r :: Length
                      , fill :: Paint
                      , stroke :: Paint
                      }
             | Image  { x :: Coordinate
                      , y :: Coordinate
                      , width :: Length
                      , height :: Length
                      , file :: IRI
                      }
             | Group  { members :: [Element]
                      }
             | Path   { fill :: Paint
                      , stroke :: Paint
                      , commands :: [PathCommand]
                      }
             deriving (Show, Eq)

data PathCommand = Move { dx :: Coordinate
                        , dy :: Coordinate
                        }
                 | Line { dx :: Coordinate
                        , dy :: Coordinate
                        }
                 | Arc  { dx :: Coordinate
                        , dy :: Coordinate
                        , rx :: Length
                        , ry :: Length
                        , xAxisRotation :: Angle
                        }
                 deriving (Show, Eq)

class Default a where
  defaultValue :: a

instance Default Color where
  defaultValue = Color.ARGB 0 0 0 0

instance Default Paint where
  defaultValue = ColorPaint defaultValue

instance Default Double where
  defaultValue = 0

circle :: Element
circle = Circle { cx = defaultValue
                , cy = defaultValue
                , r = defaultValue
                , fill = defaultValue
                , stroke = defaultValue
                }

path :: Element
path = Path { fill = defaultValue
            , stroke = defaultValue
            , commands = []
            }

rectangle :: Element
rectangle = Rectangle { x = defaultValue
                      , y = defaultValue
                      , width = defaultValue
                      , height = defaultValue
                      , cornerRadius = defaultValue
                      , fill = defaultValue
                      , stroke = defaultValue }
