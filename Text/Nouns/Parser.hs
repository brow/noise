module Text.Nouns.Parser (parseFile) where

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.String
import qualified Text.Nouns.Parser.Token as Token
import qualified Text.Nouns.Parser.AST as AST

parseFile :: String -> IO AST.SourceFile
parseFile file = do
 result <- Text.Parsec.String.parseFromFile sourceFile file 
 case result of
   Left err -> error (show err)
   Right ast -> return ast

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
