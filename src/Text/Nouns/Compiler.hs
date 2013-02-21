module Text.Nouns.Compiler
( compile
, CompileError(..)
) where

import Control.Monad.Instances ()
import qualified Text.Nouns.Parser.AST as AST
import qualified Text.Nouns.Compiler.Document as D

data CompileError = WrongNumberOfArgumentsError
                  | UndefinedFunctionError
                  deriving (Show, Eq)

compile :: AST.SourceFile -> Either CompileError D.Document
compile (AST.SourceFile funcCalls) = do
  elems <- mapM runBuiltin funcCalls
  return $ D.Document elems

runBuiltin :: AST.FunctionCall -> Either CompileError D.Element
runBuiltin (AST.FunctionCall "rectangle" [x, y, w, h]) =
  Right $ D.Rectangle (D.Length x) (D.Length y) (D.Length w) (D.Length h)
runBuiltin (AST.FunctionCall "rectangle" _) =
  Left WrongNumberOfArgumentsError
runBuiltin (AST.FunctionCall _ _) =
  Left UndefinedFunctionError
