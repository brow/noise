module Text.Nouns.Parser
( parse
, parseFile
, ParseError
) where

import Control.Monad (liftM)
import Text.ParserCombinators.Parsec (Parser, ParseError, many, sepBy1, eof)
import Text.Parsec.Prim (getPosition, try, (<|>))
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


floatValue :: Parser AST.Expression
floatValue = do
  start <- getPosition
  float <- Token.number
  end <- getPosition
  return $ AST.FloatLiteral float (start, end)

hexRGBValue :: Parser AST.Expression
hexRGBValue = do
  start <- getPosition
  hexString <- Token.hexRGB
  end <- getPosition
  return $ AST.HexRGBLiteral hexString (start, end)

value :: Parser AST.Expression
value = try hexRGBValue <|> floatValue

keywordArgument :: Parser AST.Argument
keywordArgument = do
  start <- getPosition
  keyword <- Token.identifier
  _ <- Token.symbol "="
  val <- value
  end <- getPosition
  return $ AST.KeywordArgument keyword val (start, end)

positionalArgument :: Parser AST.Argument
positionalArgument = liftM AST.PositionalArgument value

argument :: Parser AST.Argument
argument = try keywordArgument <|> positionalArgument

functionCall :: Parser AST.FunctionCall
functionCall = do
  start <- getPosition
  name <- qualifiedIdentifier
  args <- Token.parens (Token.commaSeparated argument)
  end <- getPosition
  return $ AST.FunctionCall name args (start, end)

sourceFile :: Parser AST.SourceFile
sourceFile = do
  start <- getPosition
  Token.whiteSpace
  calls <- many functionCall
  end <- getPosition
  eof >> return (AST.SourceFile calls (start, end))
