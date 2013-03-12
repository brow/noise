module Text.Nouns.Compiler
( compile
, CompileError(..)
, FunctionError(..)
) where

import Control.Monad.Instances ()
import Data.Either (partitionEithers)
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

compileArgument :: AST.Argument -> Either F.Value (String, F.Value)
compileArgument (AST.PositionalArgument value) = Left (compileValue value)
compileArgument (AST.KeywordArgument keyword value _) = Right (keyword, compileValue value)

compileValue :: AST.Expression -> F.Value
compileValue (AST.FloatLiteral x _) = F.FloatValue x
compileValue (AST.HexRGBLiteral x _) = F.RGBValue x

runBuiltin :: AST.FunctionCall -> Either CompileError D.Element
runBuiltin (AST.FunctionCall (AST.QualifiedIdentifier identifiers _) args srcRange) =
  case identifiers of
    ["shape", "rectangle"] -> use B.rectangle
    ["shape", "circle"]    -> use B.circle
    _ -> Left (UndefinedFunctionError srcRange)
  where use function = case F.call function posArgs kwArgs of
          Left callError -> Left (FunctionCallError srcRange callError)
          Right element -> Right element
        (posArgs, kwArgs) = partitionEithers $ map compileArgument args

