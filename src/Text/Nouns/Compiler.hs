module Text.Nouns.Compiler
( compile
, CompileError
) where

import Control.Monad.Instances ()
import qualified Text.Nouns.Parser.AST as AST
import Text.Nouns.Compiler.Document

data CompileError = WrongNumberOfArgumentsError
                  | UndefinedFunctionError
                  deriving Show

compile :: AST.SourceFile -> Either CompileError Document
compile (AST.SourceFile funcCalls) = do
  elems <- mapM runBuiltin funcCalls
  return $ Document elems

runBuiltin :: AST.FunctionCall -> Either CompileError Element
runBuiltin (AST.FunctionCall "rectangle" [x, y, w, h]) =
  Right $ Rectangle (Length x) (Length y) (Length w) (Length h)
runBuiltin (AST.FunctionCall "rectangle" _) =
  Left WrongNumberOfArgumentsError
runBuiltin (AST.FunctionCall _ _) =
  Left UndefinedFunctionError
