module Text.Nouns.Parser
( parse
, parseFile
, ParseError
) where

import Text.ParserCombinators.Parsec (Parser, ParseError, many, sepBy1, eof)
import Text.Parsec.Prim (getPosition)
import qualified Text.Parsec.Prim
import qualified Text.Parsec.String
import qualified Text.Nouns.Parser.Token as Token
import qualified Text.Nouns.Parser.AST as AST

parse :: String -> Either ParseError AST.SourceFile
parse = Text.Parsec.Prim.parse sourceFile ""

parseFile :: String -> IO (Either ParseError AST.SourceFile)
parseFile = Text.Parsec.String.parseFromFile sourceFile

qualifiedIdentifier :: Parser AST.QualifiedIdentifier
qualifiedIdentifier = do
  start <- getPosition
  components <- sepBy1 Token.identifier Token.dot
  end <- getPosition
  return $ AST.QualifiedIdentifier components (start, end)

argument :: Parser AST.Argument
argument = do
  start <- getPosition
  value <- Token.number
  end <- getPosition
  return $ AST.Argument value (start, end)

functionCall :: Parser AST.FunctionCall
functionCall = do
  start <- getPosition
  name <- qualifiedIdentifier
  args <- Token.parens (Token.commaSeparated argument)
  end <- getPosition
  return $ AST.FunctionCall name args (start, end)

sourceFile :: Parser AST.SourceFile
sourceFile = do
  Token.whiteSpace
  calls <- many functionCall
  eof >> return (AST.SourceFile calls)
