module Text.Nouns.Compiler.Builtin where

import qualified Text.Nouns.Compiler.Document as D
import Text.Nouns.Compiler.Function (Function, requireArg, acceptArg)

rectangle :: Function D.Element
rectangle = do
  x <- requireArg "x"
  y <- requireArg "y"
  width <- requireArg "width"
  height <- requireArg "height"
  radius <- acceptArg "radius" 0
  fill <- acceptArg "fill" D.black
  return $ D.Rectangle x y width height radius fill

circle :: Function D.Element
circle = do
  cx <- requireArg "cx"
  cy <- requireArg "cy"
  r <- requireArg "r"
  fill <- acceptArg "fill" D.black
  return $ D.Circle cx cy r fill
