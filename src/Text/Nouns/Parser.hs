module Text.Nouns.Parser
( parse
, parseFile
, ParseError
) where

import Text.ParserCombinators.Parsec (Parser, ParseError, many, eof)
import qualified Text.Parsec.Prim
import qualified Text.Parsec.String
import qualified Text.Nouns.Parser.Token as Token
import qualified Text.Nouns.Parser.AST as AST

parse :: String -> Either ParseError AST.SourceFile
parse = Text.Parsec.Prim.parse sourceFile ""

parseFile :: String -> IO (Either ParseError AST.SourceFile)
parseFile = Text.Parsec.String.parseFromFile sourceFile

functionCall :: Parser AST.FunctionCall
functionCall = do
  name <- Token.identifier
  args <- Token.parens (Token.commaSeparated Token.number)
  return $ AST.FunctionCall name args

sourceFile :: Parser AST.SourceFile
sourceFile = do
  Token.whiteSpace
  calls <- many functionCall
  eof >> return (AST.SourceFile calls)
