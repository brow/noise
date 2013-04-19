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

data CompileError = FunctionCallError AST.FunctionCall FunctionError
                  | UndefinedFunctionError AST.QualifiedIdentifier
                  | StatementReturnTypeError AST.FunctionCall
                  deriving (Show, Eq)

instance HasSourceRange CompileError where
  rangeInSource (FunctionCallError fnCall _) = rangeInSource fnCall
  rangeInSource (UndefinedFunctionError identifier) = rangeInSource identifier
  rangeInSource (StatementReturnTypeError fnCall) = rangeInSource fnCall

instance Error.Error CompileError where
  message compileError = case compileError of
    UndefinedFunctionError identifier ->
      "Undefined function \"" ++ showDotSyntax identifier ++ "\"."
    StatementReturnTypeError (AST.FunctionCall identifier _ _) ->
      "Function \"" ++ showDotSyntax identifier ++ "\" does not return an element."
    FunctionCallError (AST.FunctionCall identifier _ _) (MissingArgumentError keyword) ->
      "Function \"" ++ showDotSyntax identifier ++ "\" requires argument \"" ++ keyword ++ "\"."
    FunctionCallError (AST.FunctionCall identifier _ _) (ArgumentTypeError keyword) ->
      "Argument \"" ++ keyword ++ "\" to function \"" ++ showDotSyntax identifier ++ "\" has incorrect type."
    FunctionCallError (AST.FunctionCall identifier _ _) TooManyArgumentsError ->
      "Too many arguments to function \"" ++ showDotSyntax identifier ++ "\"."
    where showDotSyntax (AST.QualifiedIdentifier components _) =
            List.intercalate "." components

type Compiled a = Either CompileError a

type Definitions = Map.Map [AST.Identifier] (F.Function F.Value)

data CompileState = CompileState Definitions [D.Element]

emptyCompileState :: CompileState
emptyCompileState = CompileState Builtin.definitions []

compile :: AST.SourceFile -> Compiled D.Document
compile (AST.SourceFile statements _) = do
  CompileState _ elems <- compileStatements statements
  return $ D.Document elems

compileStatements :: [AST.Statement] -> Compiled CompileState
compileStatements = Monad.foldM compileStatement emptyCompileState

compileStatement :: CompileState -> AST.Statement -> Compiled CompileState
compileStatement (CompileState defs elems) (AST.FunctionCallStatement fnCall) = do
  value <- compileFunctionCall defs fnCall
  case value of
    F.ElementValue element -> Right $ CompileState defs (elems ++ [element])
    _                      -> Left $ StatementReturnTypeError fnCall
compileStatement (CompileState defs elems) (AST.FunctionDefStatement prototype definition _) = do
  value <- compileExp defs definition
  return $ CompileState (Map.insert identComponents (return value) defs) elems
  where (AST.QualifiedIdentifier identComponents _) = identifier
        (AST.FunctionPrototype identifier _ _) = prototype

compileFunctionCall :: Definitions -> AST.FunctionCall -> Compiled F.Value
compileFunctionCall defs functionCall@(AST.FunctionCall identifier args _) = do
  function <- lookUpFunction defs identifier
  (posArgs, kwArgs) <- compileArguments defs args
  case F.call function posArgs kwArgs of
    Left callError -> Left (FunctionCallError functionCall callError)
    Right value    -> Right value

lookUpFunction :: Definitions -> AST.QualifiedIdentifier -> Compiled (F.Function F.Value)
lookUpFunction defs identifier@(AST.QualifiedIdentifier path _) =
  case Map.lookup path defs of
    Just fn -> Right fn
    Nothing -> Left (UndefinedFunctionError identifier)

compileArguments :: Definitions -> [AST.Argument] -> Compiled ([F.Value], [(String, F.Value)])
compileArguments defs = fmap partitionEithers . mapM (compileArgument defs)

compileArgument :: Definitions -> AST.Argument -> Compiled (Either F.Value (String, F.Value))
compileArgument defs (AST.PositionalArgument valueExp) = do
  value <- compileExp defs valueExp
  return $ Left value
compileArgument defs (AST.KeywordArgument keyword valueExp _) = do
  value <- compileExp defs valueExp
  return $ Right (keyword, value)

compileExp :: Definitions -> AST.Expression -> Compiled F.Value
compileExp _ (AST.FloatLiteral x _) = return (F.FloatValue x)
compileExp _ (AST.HexRGBLiteral x _) = return (F.RGBValue x)
compileExp _ (AST.StringLiteral x _) = return (F.StringValue x)
compileExp defs (AST.FunctionCallExp fnCall) = compileFunctionCall defs fnCall
