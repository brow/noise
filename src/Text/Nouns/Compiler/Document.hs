module Text.Nouns.Compiler.Document
( Number
, Angle
, Coordinate
, Length
, IRI
, Paint(..)
, Color(..)
, Gradient(..)
, Document(..)
, Element(..)
, PathCommand(..)
, black
, showFuncIRI
, localIRIForId
, fileIRI
) where

import qualified Network.URI as URI

type Number = Double

type Angle = Number

type Length = Number

type Coordinate = Length

type IRI = URI.URI

showFuncIRI :: IRI -> String
showFuncIRI iri = "url(" ++ show iri ++ ")"

isUnescapedInURIComponent :: Char -> Bool
isUnescapedInURIComponent c = not (URI.isReserved c || not (URI.isUnescapedInURI c))

localIRIForId :: String -> IRI
localIRIForId id' = URI.nullURI { URI.uriFragment = fragment }
  where fragment = '#' : URI.escapeURIString isUnescapedInURIComponent id'

fileIRI :: String -> Maybe IRI
fileIRI = URI.parseRelativeReference

newtype Color = Color { hexString :: String } deriving (Eq)

instance Show Color where
  show color = '#' : hexString color

data Gradient = LinearGradient { angle :: Angle
                               , stops :: [(Number,Color)]
                               }
              | RadialGradient { stops :: [(Number,Color)]
                               }
              deriving (Eq, Show)

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
             | Image { x :: Coordinate
                     , y :: Coordinate
                     , width :: Length
                     , height :: Length
                     , file :: IRI
                     }
             | Group { members :: [Element]
                     }
             | Path { fill :: Paint
                    , commands :: [PathCommand]
                    }
             deriving (Show, Eq)

data PathCommand = Move Coordinate Coordinate
                 | Line Coordinate Coordinate
                 deriving (Show, Eq)
