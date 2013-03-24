module Text.Nouns.Compiler.Builtin
( functionWithName
) where

import Control.Applicative
import qualified Text.Nouns.Compiler.Document as D
import qualified Text.Nouns.Compiler.Function as F
import Text.Nouns.Compiler.Function (Function, requireArg, acceptArg)

functionWithName :: [String] -> Maybe (Function F.Value)
functionWithName name = case name of
  "shape" : x -> case x of
    ["rectangle"]   -> Just rectangle
    ["circle"]      -> Just circle
    _               -> Nothing
  "color" : x -> case x of
    ["red"]         -> Just (color "ff0000")
    ["green"]       -> Just (color "00ff00")
    ["blue"]        -> Just (color "0000ff")
    _               -> Nothing
  "gradient" : x -> case x of
    ["vertical"]    -> Just verticalGradient
    ["horizontal"]  -> Just horizontalGradient
    ["radial"]      -> Just radialGradient
    _               -> Nothing
  ["image"]         -> Just image
  _                 -> Nothing

rectangle :: Function F.Value
rectangle = fmap F.ElementValue $ D.Rectangle
  <$> requireArg "x"
  <*> requireArg "y"
  <*> requireArg "width"
  <*> requireArg "height"
  <*> acceptArg "radius" 0
  <*> acceptArg "fill" D.black

circle :: Function F.Value
circle = fmap F.ElementValue $ D.Circle
  <$> requireArg "cx"
  <*> requireArg "cy"
  <*> requireArg "r"
  <*> acceptArg "fill" D.black

color :: String -> Function F.Value
color = return . F.RGBValue

requireGradientColorArgs :: Function [(D.Number,D.Color)]
requireGradientColorArgs = do
  from <- requireArg "from"
  to <- requireArg "to"
  return [(0, from), (1, to)]

verticalGradient :: Function F.Value
verticalGradient = fmap F.GradientValue $ D.LinearGradient 90
  <$> requireGradientColorArgs

horizontalGradient :: Function F.Value
horizontalGradient = fmap F.GradientValue $ D.LinearGradient 0
  <$> requireGradientColorArgs

radialGradient :: Function F.Value
radialGradient = fmap F.GradientValue $ D.RadialGradient
  <$> requireGradientColorArgs

image :: Function F.Value
image = fmap F.ElementValue $ D.Image
  <$> requireArg "x"
  <*> requireArg "y"
  <*> requireArg "width"
  <*> requireArg "height"
  <*> requireArg "file"
