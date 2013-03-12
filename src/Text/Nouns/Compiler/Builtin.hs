module Text.Nouns.Compiler.Builtin
( functionWithName
) where

import qualified Text.Nouns.Compiler.Document as D
import qualified Text.Nouns.Compiler.Function as F
import Text.Nouns.Compiler.Function (Function, requireArg, acceptArg)

functionWithName :: [String] -> Maybe (Function F.Value)
functionWithName name = case name of
  "shape" : x -> case x of
    ["rectangle"] -> Just rectangle
    ["circle"]    -> Just circle
    _             -> Nothing
  "color" : x -> case x of
    ["red"]       -> Just (color "ff0000")
    ["green"]     -> Just (color "00ff00")
    ["blue"]      -> Just (color "0000ff")
    _             -> Nothing
  _               -> Nothing

rectangle :: Function F.Value
rectangle = do
  x <- requireArg "x"
  y <- requireArg "y"
  width <- requireArg "width"
  height <- requireArg "height"
  radius <- acceptArg "radius" 0
  fill <- acceptArg "fill" D.black
  return $ F.ElementValue $ D.Rectangle x y width height radius fill

circle :: Function F.Value
circle = do
  cx <- requireArg "cx"
  cy <- requireArg "cy"
  r <- requireArg "r"
  fill <- acceptArg "fill" D.black
  return $ F.ElementValue $ D.Circle cx cy r fill

color :: String -> Function F.Value
color = return . F.RGBValue
