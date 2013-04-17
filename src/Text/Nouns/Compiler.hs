module Text.Nouns.Compiler
( compile
, CompileError(..)
, FunctionError(..)
) where

import Control.Monad.Instances ()
import Data.Either (partitionEithers)
import qualified Data.List as List
import qualified Text.Nouns.Parser.AST as AST
import qualified Text.Nouns.Compiler.Document as D
import qualified Text.Nouns.Compiler.Function as F
import qualified Text.Nouns.Compiler.Builtin as Builtin
import qualified Text.Nouns.Error as Error
import Text.Nouns.SourceRange (HasSourceRange, rangeInSource)
import Text.Nouns.Compiler.Function (FunctionError(..))

data CompileError = FunctionCallError AST.FunctionCall FunctionError
                  | UndefinedFunctionError AST.QualifiedIdentifier
                  | StatementReturnTypeError AST.FunctionCall
                  deriving (Show, Eq)

instance HasSourceRange CompileError where
  rangeInSource (FunctionCallError ast _) = rangeInSource ast
  rangeInSource (UndefinedFunctionError ast) = rangeInSource ast
  rangeInSource (StatementReturnTypeError ast) = rangeInSource ast

instance Error.Error CompileError where
  message compileError = case compileError of
    UndefinedFunctionError identifier ->
      "Undefined function \"" ++ showDotSyntax identifier ++ "\"."
    StatementReturnTypeError (AST.FunctionCall identifier _ _) ->
      "Function \"" ++ showDotSyntax identifier ++ "\" does not return an element."
    FunctionCallError _ (MissingArgumentError keyword) ->
      "Missing '" ++ keyword ++ "' argument."
    FunctionCallError _ (ArgumentTypeError keyword) ->
      "Argument '" ++ keyword ++ "' has the wrong type."
    FunctionCallError _ TooManyArgumentsError ->
      "Too many arguments."
    where showDotSyntax (AST.QualifiedIdentifier components _) =
            List.intercalate "." components

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
    _                      -> Left (StatementReturnTypeError fnCall)

compileFunctionCall :: AST.FunctionCall -> Compiled F.Value
compileFunctionCall functionCall@(AST.FunctionCall name args _) = do
  function <- compileFunctionName name
  (posArgs, kwArgs) <- compileArguments args
  case F.call function posArgs kwArgs of
    Left callError -> Left (FunctionCallError functionCall callError)
    Right value    -> Right value

compileFunctionName :: AST.QualifiedIdentifier -> Compiled (F.Function F.Value)
compileFunctionName identifier@(AST.QualifiedIdentifier components _) =
  case Builtin.functionWithName components of
    Just function -> Right function
    _             -> Left (UndefinedFunctionError identifier)

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
