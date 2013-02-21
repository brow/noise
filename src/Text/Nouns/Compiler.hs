module Text.Nouns.Compiler
( compile
, CompileError(..)
, FunctionError(..)
) where

import Control.Monad.Instances ()
import qualified Text.Nouns.Parser.AST as AST
import qualified Text.Nouns.Compiler.Document as D
import qualified Text.Nouns.Compiler.Function as F
import qualified Text.Nouns.Compiler.Builtin as B
import Text.Nouns.Compiler.Function (FunctionError(..))

data CompileError = FunctionCallError FunctionError
                  | UndefinedFunctionError
                  deriving (Show, Eq)

compile :: AST.SourceFile -> Either CompileError D.Document
compile (AST.SourceFile funcCalls) = do
  elems <- mapM runBuiltin funcCalls
  return $ D.Document elems

runBuiltin :: AST.FunctionCall -> Either CompileError D.Element
runBuiltin (AST.FunctionCall name args) =
  case name of
    "rectangle" -> use B.rectangle
    "circle" -> use B.circle
    _ -> Left UndefinedFunctionError
  where use function = case F.call function args of
          Left callError -> Left (FunctionCallError callError)
          Right element -> Right element
