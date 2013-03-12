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
compile (AST.SourceFile fnCalls _) = do
  elems <- mapM compileFunctionCall fnCalls
  return $ D.Document elems

compileFunctionCall :: AST.FunctionCall -> Either CompileError D.Element
compileFunctionCall (AST.FunctionCall name args srcRange) = do
  function <- compileFunctionName name
  (posArgs, kwArgs) <- compileArguments args
  case F.call function posArgs kwArgs of
    Left callError -> Left $ FunctionCallError srcRange callError
    Right element -> Right element

compileFunctionName :: AST.QualifiedIdentifier -> Either CompileError (F.Function D.Element)
compileFunctionName (AST.QualifiedIdentifier components srcRange) =
  case components of
    ["shape", "rectangle"] -> return B.rectangle
    ["shape", "circle"]    -> return B.circle
    _ -> Left $ UndefinedFunctionError srcRange

compileArguments :: [AST.Argument] -> Either CompileError ([F.Value], [(String, F.Value)])
compileArguments = fmap partitionEithers . mapM compileArgument

compileArgument :: AST.Argument -> Either CompileError (Either F.Value (String, F.Value))
compileArgument (AST.PositionalArgument valueExp) = do
  value <- compileExp valueExp
  return $ Left value
compileArgument (AST.KeywordArgument keyword valueExp _) = do
  value <- compileExp valueExp
  return $ Right (keyword, value)

compileExp :: AST.Expression -> Either CompileError F.Value
compileExp (AST.FloatLiteral x _) = return (F.FloatValue x)
compileExp (AST.HexRGBLiteral x _) = return (F.RGBValue x)
