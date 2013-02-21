module Text.Nouns.Compiler.Builtin where

import qualified Text.Nouns.Compiler.Document as D
import Text.Nouns.Compiler.Function (Function, requireArg)

rectangle :: Function D.Element
rectangle = do
  x <- requireArg
  y <- requireArg
  width <- requireArg
  height <- requireArg
  return $ D.Rectangle x y width height

circle :: Function D.Element
circle = do
  cx <- requireArg
  cy <- requireArg
  r <- requireArg
  return $ D.Circle cx cy r
