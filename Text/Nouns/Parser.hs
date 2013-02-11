module Text.Nouns.Parser (sourceFile) where

import Text.ParserCombinators.Parsec
import qualified Text.Nouns.Parser.Token as Token
import qualified Text.Nouns.Parser.AST as AST

functionCall :: Parser AST.FunctionCall
functionCall = do
  name <- Token.identifier
  args <- Token.parens (Token.commaSeparated Token.integer)
  return $ AST.FunctionCall name args

sourceFile :: Parser AST.SourceFile
sourceFile = do
  Token.whiteSpace
  calls <- many functionCall
  eof >> return (AST.SourceFile calls)
