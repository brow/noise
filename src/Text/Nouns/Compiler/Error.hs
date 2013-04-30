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
                   | BlockStatementTypeError
                   | TooManyArgumentsError
                   | CompileError CompileError
                   | RedundantKeywordArgError String
                   deriving (Show, Eq)

data CompileError = FunctionCallError AST.QualifiedIdentifier FunctionError
                  | UndefinedFunctionError AST.QualifiedIdentifier
                  | ExpressionStatementTypeError AST.Expression
                  | PositionalArgumentError AST.Argument
                  | DuplicatedArgumentPrototypeError AST.ArgumentPrototype
                  | DuplicatedKeywordArgumentError AST.Argument
                  deriving (Show, Eq)

instance HasSourceRange CompileError where
  rangeInSource err = case err of
    DuplicatedArgumentPrototypeError arg -> rangeInSource arg
    ExpressionStatementTypeError fnCall -> rangeInSource fnCall
    DuplicatedKeywordArgumentError arg -> rangeInSource arg
    UndefinedFunctionError identifier -> rangeInSource identifier
    PositionalArgumentError arg -> rangeInSource arg
    FunctionCallError _ (CompileError err') -> rangeInSource err'
    FunctionCallError fnCall _ -> rangeInSource fnCall
instance Error.Error CompileError where

  message err =
    let showDotSyntax (AST.QualifiedIdentifier path _) = List.intercalate "." path
        showArgPrototypeName (AST.RequiredArgumentPrototype name _) = name
        showArgName (AST.KeywordArgument keyword _ _) = keyword
        showArgName _ = ""
    in case err of
      UndefinedFunctionError identifier ->
        "Undefined function \"" ++ showDotSyntax identifier ++ "\"."
      ExpressionStatementTypeError _ ->
        "Top-level expression is not an element."
      PositionalArgumentError _ ->
        "Positional argument follows a keyword argument."
      DuplicatedArgumentPrototypeError arg ->
        "Duplicate argument \"" ++ showArgPrototypeName arg ++ "\" in function definition."
      DuplicatedKeywordArgumentError arg ->
        "Duplicate keyword argument \"" ++ showArgName arg ++ "\" in function call."
      FunctionCallError identifier fnCallErr -> case fnCallErr of
        RedundantKeywordArgError keyword ->
          "Keyword argument \"" ++ keyword ++ "\" duplicates a positional argument."
        MissingArgumentError keyword ->
          "Function \"" ++ fnName ++ "\" requires argument \"" ++ keyword ++ "\"."
        ArgumentTypeError keyword ->
          "Argument \"" ++ keyword ++ "\" to function \"" ++ fnName ++ "\" has incorrect type."
        BlockStatementTypeError ->
          "Statement in block of function \"" ++ fnName ++ "\" has incorrect type."
        TooManyArgumentsError ->
          "Too many arguments to function \"" ++ fnName ++ "\"."
        CompileError err' ->
          Error.message err'
        where fnName = showDotSyntax identifier
