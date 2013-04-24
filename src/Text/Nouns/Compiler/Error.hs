module Text.Nouns.Compiler.Error
( FunctionError(..)
, CompileError(..)
) where

import qualified Data.List as List
import qualified Text.Nouns.Parser.AST as AST
import qualified Text.Nouns.Error as Error
import Text.Nouns.SourceRange (HasSourceRange, rangeInSource)

data FunctionError = MissingArgumentError String
                   | ArgumentTypeError String
                   | TooManyArgumentsError
                   | CompileError CompileError
                   deriving (Show, Eq)

data CompileError = FunctionCallError AST.QualifiedIdentifier FunctionError
                  | UndefinedFunctionError AST.QualifiedIdentifier
                  | ExpressionStatementTypeError AST.Expression
                  deriving (Show, Eq)

instance HasSourceRange CompileError where
  rangeInSource (FunctionCallError _ (CompileError err)) = rangeInSource err
  rangeInSource (FunctionCallError fnCall _) = rangeInSource fnCall
  rangeInSource (UndefinedFunctionError identifier) = rangeInSource identifier
  rangeInSource (ExpressionStatementTypeError fnCall) = rangeInSource fnCall

instance Error.Error CompileError where
  message compileError =
    let showDotSyntax (AST.QualifiedIdentifier path _) = List.intercalate "." path in
    case compileError of
      UndefinedFunctionError identifier ->
        "Undefined function \"" ++ showDotSyntax identifier ++ "\"."
      ExpressionStatementTypeError _ ->
        "Top-level expression is not an element."
      FunctionCallError identifier functionError -> case functionError of
        MissingArgumentError keyword ->
          "Function \"" ++ fnName ++ "\" requires argument \"" ++ keyword ++ "\"."
        ArgumentTypeError keyword ->
          "Argument \"" ++ keyword ++ "\" to function \"" ++ fnName ++ "\" has incorrect type."
        TooManyArgumentsError ->
          "Too many arguments to function \"" ++ fnName ++ "\"."
        CompileError err ->
          Error.message err
        where fnName = showDotSyntax identifier
