module Text.Nouns.Compiler
( compile
, CompileError(..)
, FunctionError(..)
) where

import Data.Function
import Data.Either (partitionEithers)
import Control.Monad
import Control.Applicative
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Text.Nouns.Parser.AST as AST
import qualified Text.Nouns.Compiler.Document as D
import qualified Text.Nouns.Compiler.Function as F
import qualified Text.Nouns.Compiler.Builtin as Builtin
import Text.Nouns.Compiler.Error (CompileError(..), FunctionError(..))

type Compiled a = Either CompileError a

type Definitions = Map.Map AST.IdentifierPath (F.Function F.Value)

data CompileState = CompileState Definitions [D.Element]

throw :: CompileError -> Compiled a
throw = Left

initialCompileState :: CompileState
initialCompileState = CompileState Builtin.definitions []

compile :: AST.SourceFile -> Compiled D.Document
compile (AST.SourceFile statements _) = do
  CompileState _ elems <- compileStatements statements
  return $ D.Document elems

compileStatements :: [AST.Statement] -> Compiled CompileState
compileStatements = foldM compileStatement initialCompileState

compileStatement :: CompileState -> AST.Statement -> Compiled CompileState
compileStatement (CompileState defs elems) (AST.ExpressionStatement expression) = do
  value <- evaluate defs expression
  case value of
    F.ElementValue element -> return $ CompileState defs (elems ++ [element])
    _                      -> throw $ ExpressionStatementTypeError expression
compileStatement (CompileState defs elems) (AST.DefinitionStatement fnPrototype expression _) = do
  (functionPath, argNames) <- compileFunctionPrototype fnPrototype
  let definition = compileFunctionDef defs argNames expression
  return $ CompileState (Map.insert functionPath definition defs) elems

compileFunctionDef :: Definitions -> [AST.Identifier] -> AST.Expression -> F.Function F.Value
compileFunctionDef defs argNames expression = do
  argValues <- map return <$> mapM F.requireArg argNames
  let argDefs = Map.fromList $ zip (map return argNames) argValues
  let localDefs = argDefs `Map.union` defs
  case evaluate localDefs expression of
    Left err    -> F.throw (F.CompileError err)
    Right value -> return value
  where

compileFunctionPrototype :: AST.FunctionPrototype -> Compiled (AST.IdentifierPath, [AST.Identifier])
compileFunctionPrototype (AST.FunctionPrototype (AST.QualifiedIdentifier path _) argPrototypes _) =
  case duplicateArg of
    Just arg -> throw (DuplicatedArgumentPrototypeError arg)
    Nothing  -> return (path, argNames)
  where
    argNames = [name | AST.RequiredArgumentPrototype name _ <- argPrototypes]
    duplicateArg = firstDuplicateBy ((==) `on` argName) argPrototypes
    argName (AST.RequiredArgumentPrototype n _) = n

firstDuplicateBy :: (a -> a -> Bool) -> [a] -> Maybe a
firstDuplicateBy cmp (x:xs) = case List.find (cmp x) xs of
  Nothing -> firstDuplicateBy cmp xs
  Just x' -> Just x'
firstDuplicateBy _ [] = Nothing

evaluate :: Definitions -> AST.Expression -> Compiled F.Value
evaluate _ (AST.FloatLiteral x _) = return (F.FloatValue x)
evaluate _ (AST.HexRGBLiteral x _) = return (F.RGBValue x)
evaluate _ (AST.StringLiteral x _) = return (F.StringValue x)
evaluate defs (AST.FunctionCall identifier args _) = do
  function <- lookUpFunction defs identifier
  (posArgs, kwArgs) <- evaluateArguments defs args
  case F.call function posArgs kwArgs of
    Left callError -> throw (FunctionCallError identifier callError)
    Right value    -> return value

lookUpFunction :: Definitions -> AST.QualifiedIdentifier -> Compiled (F.Function F.Value)
lookUpFunction defs identifier@(AST.QualifiedIdentifier path _) =
  case Map.lookup path defs of
    Just fn -> return fn
    Nothing -> throw (UndefinedFunctionError identifier)

evaluateArguments :: Definitions -> [AST.Argument] -> Compiled ([F.Value], [(String, F.Value)])
evaluateArguments defs args =
  let trailingPosArgs = dropWhile (not . isPosArg) $ dropWhile isPosArg args
  in if null trailingPosArgs
    then partitionEithers <$> mapM (evaluateArgument defs) args
    else throw $ PositionalArgumentError (head trailingPosArgs)
  where isPosArg (AST.PositionalArgument _) = True
        isPosArg _ = False

evaluateArgument :: Definitions -> AST.Argument -> Compiled (Either F.Value (String, F.Value))
evaluateArgument defs (AST.PositionalArgument expression) =
  Left <$> evaluate defs expression
evaluateArgument defs (AST.KeywordArgument keyword expression _) =
  Right . (,) keyword <$> evaluate defs expression
