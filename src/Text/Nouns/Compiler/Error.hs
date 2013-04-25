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
                  | PositionalArgumentError AST.Argument
                  | DuplicatedArgumentPrototypeError AST.ArgumentPrototype
                  deriving (Show, Eq)

instance HasSourceRange CompileError where
  rangeInSource err = case err of
    FunctionCallError _ (CompileError err') -> rangeInSource err'
    DuplicatedArgumentPrototypeError arg -> rangeInSource arg
    ExpressionStatementTypeError fnCall -> rangeInSource fnCall
    UndefinedFunctionError identifier -> rangeInSource identifier
    PositionalArgumentError arg -> rangeInSource arg
    FunctionCallError fnCall _ -> rangeInSource fnCall

instance Error.Error CompileError where
  message err =
    let showDotSyntax (AST.QualifiedIdentifier path _) = List.intercalate "." path
        showArgName (AST.RequiredArgumentPrototype name _) = name
    in case err of
      UndefinedFunctionError identifier ->
        "Undefined function \"" ++ showDotSyntax identifier ++ "\"."
      ExpressionStatementTypeError _ ->
        "Top-level expression is not an element."
      PositionalArgumentError _ ->
        "Positional argument follows a keyword argument."
      DuplicatedArgumentPrototypeError arg ->
        "Duplicate argument \"" ++ showArgName arg ++ "\" in function definition."
      FunctionCallError identifier fnCallErr -> case fnCallErr of
        MissingArgumentError keyword ->
          "Function \"" ++ fnName ++ "\" requires argument \"" ++ keyword ++ "\"."
        ArgumentTypeError keyword ->
          "Argument \"" ++ keyword ++ "\" to function \"" ++ fnName ++ "\" has incorrect type."
        TooManyArgumentsError ->
          "Too many arguments to function \"" ++ fnName ++ "\"."
        CompileError err' ->
          Error.message err'
        where fnName = showDotSyntax identifier
