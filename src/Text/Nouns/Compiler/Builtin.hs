module Text.Nouns.Compiler.Builtin
( definitions
) where

import Control.Applicative
import qualified Data.Map as Map
import qualified Text.Nouns.Compiler.Document as D
import qualified Text.Nouns.Compiler.Function as F
import Text.Nouns.Compiler.Function (Function, requireArg, acceptArg, acceptBlockArgs)

definitions :: Map.Map [String] (Function F.Value)
definitions = Map.fromList
  [ (["shape","rectangle"],     rectangle)
  , (["shape","circle"],        circle)
  , (["color","red"],           color "ff0000")
  , (["color","green"],         color "00ff00")
  , (["color","blue"],          color "0000ff")
  , (["gradient","vertical"],   linearGradient 90)
  , (["gradient","horizontal"], linearGradient 0)
  , (["gradient","radial"],     radialGradient)
  , (["image"],                 image)
  , (["group"],                 group)
  ]

rectangle :: Function F.Value
rectangle =  fmap F.ElementValue $ D.Rectangle
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
  <*> requireArg "radius"
  <*> acceptArg "fill" D.black

color :: String -> Function F.Value
color = return . F.RGBValue

requireGradientColorArgs :: Function [(D.Number,D.Color)]
requireGradientColorArgs = do
  from <- requireArg "from"
  to <- requireArg "to"
  return [(0, from), (1, to)]

linearGradient :: D.Angle -> Function F.Value
linearGradient angle = fmap F.GradientValue $ D.LinearGradient angle
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

group :: Function F.Value
group = fmap F.ElementValue $ D.Group
  <$> acceptBlockArgs
