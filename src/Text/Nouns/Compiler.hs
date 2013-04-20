module Text.Nouns.Compiler ( compile
, CompileError(..)
, FunctionError(..)
) where

import Control.Monad.Instances ()
import Data.Either (partitionEithers)
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Control.Monad as Monad
import qualified Text.Nouns.Parser.AST as AST
import qualified Text.Nouns.Compiler.Document as D
import qualified Text.Nouns.Compiler.Function as F
import qualified Text.Nouns.Compiler.Builtin as Builtin
import qualified Text.Nouns.Error as Error
import Text.Nouns.SourceRange (HasSourceRange, rangeInSource)
import Text.Nouns.Compiler.Function (FunctionError(..))

data CompileError = FunctionCallError AST.QualifiedIdentifier FunctionError
                  | UndefinedFunctionError AST.QualifiedIdentifier
                  | ExpressionStatementTypeError AST.Expression
                  deriving (Show, Eq)

instance HasSourceRange CompileError where
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
        where fnName = showDotSyntax identifier

type Compiled a = Either CompileError a

type Definitions = Map.Map [AST.Identifier] (F.Function F.Value)

data CompileState = CompileState Definitions [D.Element]

initialCompileState :: CompileState
initialCompileState = CompileState Builtin.definitions []

compile :: AST.SourceFile -> Compiled D.Document
compile (AST.SourceFile statements _) = do
  CompileState _ elems <- compileStatements statements
  return $ D.Document elems

compileStatements :: [AST.Statement] -> Compiled CompileState
compileStatements = Monad.foldM compileStatement initialCompileState

compileStatement :: CompileState -> AST.Statement -> Compiled CompileState
compileStatement (CompileState defs elems) (AST.ExpressionStatement expression) = do
  value <- evaluate defs expression
  case value of
    F.ElementValue element -> Right $ CompileState defs (elems ++ [element])
    _                      -> Left $ ExpressionStatementTypeError expression
compileStatement (CompileState defs elems) (AST.DefinitionStatement prototype definition _) = do
  value <- evaluate defs definition
  return $ CompileState (Map.insert identComponents (return value) defs) elems
  where (AST.QualifiedIdentifier identComponents _) = identifier
        (AST.FunctionPrototype identifier _ _) = prototype

evaluate :: Definitions -> AST.Expression -> Compiled F.Value
evaluate _ (AST.FloatLiteral x _) = return (F.FloatValue x)
evaluate _ (AST.HexRGBLiteral x _) = return (F.RGBValue x)
evaluate _ (AST.StringLiteral x _) = return (F.StringValue x)
evaluate defs (AST.FunctionCall identifier args _) = do
  function <- lookUpFunction defs identifier
  (posArgs, kwArgs) <- evaluateArguments defs args
  case F.call function posArgs kwArgs of
    Left callError -> Left (FunctionCallError identifier callError)
    Right value    -> Right value

lookUpFunction :: Definitions -> AST.QualifiedIdentifier -> Compiled (F.Function F.Value)
lookUpFunction defs identifier@(AST.QualifiedIdentifier path _) =
  case Map.lookup path defs of
    Just fn -> Right fn
    Nothing -> Left (UndefinedFunctionError identifier)

evaluateArguments :: Definitions -> [AST.Argument] -> Compiled ([F.Value], [(String, F.Value)])
evaluateArguments defs = fmap partitionEithers . mapM (evaluateArgument defs)

evaluateArgument :: Definitions -> AST.Argument -> Compiled (Either F.Value (String, F.Value))
evaluateArgument defs (AST.PositionalArgument valueExp) = do
  value <- evaluate defs valueExp
  return $ Left value
evaluateArgument defs (AST.KeywordArgument keyword valueExp _) = do
  value <- evaluate defs valueExp
  return $ Right (keyword, value)

