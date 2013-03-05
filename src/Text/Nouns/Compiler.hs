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

data CompileError = FunctionCallError AST.SourceRange FunctionError
                  | UndefinedFunctionError AST.SourceRange
                  deriving (Show, Eq)

compile :: AST.SourceFile -> Either CompileError D.Document
compile (AST.SourceFile funcCalls _) = do
  elems <- mapM runBuiltin funcCalls
  return $ D.Document elems

compileArgument :: AST.Argument -> F.Value
compileArgument (AST.Argument val _) = val

runBuiltin :: AST.FunctionCall -> Either CompileError D.Element
runBuiltin (AST.FunctionCall (AST.QualifiedIdentifier identifiers _) args srcRange) =
  case identifiers of
    ["shape", "rectangle"] -> use B.rectangle
    ["shape", "circle"]    -> use B.circle
    _ -> Left (UndefinedFunctionError srcRange)
  where use function = case F.call function values of
          Left callError -> Left (FunctionCallError srcRange callError)
          Right element -> Right element
        values = map compileArgument args
