module Text.Nouns.Compiler.Builtin where

import qualified Text.Nouns.Compiler.Document as D
import Text.Nouns.Compiler.Function (Function, requireArg)

rectangle :: Function D.Element
rectangle = do
  x <- requireArg
  y <- requireArg
  width <- requireArg
  height <- requireArg
  return $ D.Rectangle (D.Length x) (D.Length y) (D.Length width) (D.Length height)
