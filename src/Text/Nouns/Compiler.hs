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
import qualified Text.Nouns.Compiler.Builtin as Builtin
import Text.Nouns.Compiler.Function (FunctionError(..))

data CompileError = FunctionCallError AST.SourceRange FunctionError
                  | UndefinedFunctionError AST.SourceRange
                  | StatementReturnTypeError AST.SourceRange
                  deriving (Show, Eq)

type Compiled a = Either CompileError a

compile :: AST.SourceFile -> Compiled D.Document
compile (AST.SourceFile fnCalls _) = do
  elems <- mapM compileStatement fnCalls
  return $ D.Document elems

compileStatement :: AST.Statement -> Compiled D.Element
compileStatement (AST.FunctionCallStatement fnCall) = do
  value <- compileFunctionCall fnCall
  case value of
    F.ElementValue element -> return element
    _ -> Left $ StatementReturnTypeError (AST.rangeInSource fnCall)

compileFunctionCall :: AST.FunctionCall -> Compiled F.Value
compileFunctionCall (AST.FunctionCall name args srcRange) = do
  function <- compileFunctionName name
  (posArgs, kwArgs) <- compileArguments args
  case F.call function posArgs kwArgs of
    Left callError -> Left $ FunctionCallError srcRange callError
    Right value -> Right value

compileFunctionName :: AST.QualifiedIdentifier -> Compiled (F.Function F.Value)
compileFunctionName (AST.QualifiedIdentifier components srcRange) =
  case Builtin.functionWithName components of
    Just function -> Right function
    _ -> Left $ UndefinedFunctionError srcRange

compileArguments :: [AST.Argument] -> Compiled ([F.Value], [(String, F.Value)])
compileArguments = fmap partitionEithers . mapM compileArgument

compileArgument :: AST.Argument -> Compiled (Either F.Value (String, F.Value))
compileArgument (AST.PositionalArgument valueExp) = do
  value <- compileExp valueExp
  return $ Left value
compileArgument (AST.KeywordArgument keyword valueExp _) = do
  value <- compileExp valueExp
  return $ Right (keyword, value)

compileExp :: AST.Expression -> Compiled F.Value
compileExp (AST.FloatLiteral x _) = return (F.FloatValue x)
compileExp (AST.HexRGBLiteral x _) = return (F.RGBValue x)
compileExp (AST.StringLiteral x _) = return (F.StringValue x)
compileExp (AST.FunctionCallExp fnCall) = compileFunctionCall fnCall
