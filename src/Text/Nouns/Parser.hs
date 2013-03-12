module Text.Nouns.Parser
( parse
, parseFile
, ParseError
) where

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


floatLiteral :: Parser AST.Expression
floatLiteral = do
  start <- getPosition
  float <- Token.number
  end <- getPosition
  return $ AST.FloatLiteral float (start, end)

hexRGBLiteral :: Parser AST.Expression
hexRGBLiteral = do
  start <- getPosition
  hexString <- Token.hexRGB
  end <- getPosition
  return $ AST.HexRGBLiteral hexString (start, end)

functionCallExp :: Parser AST.Expression
functionCallExp = fmap AST.FunctionCallExp functionCall

expression :: Parser AST.Expression
expression = try hexRGBLiteral <|>
             try floatLiteral <|>
             functionCallExp

statement :: Parser AST.Statement
statement = fmap AST.FunctionCallStatement functionCall

keywordArgument :: Parser AST.Argument
keywordArgument = do
  start <- getPosition
  keyword <- Token.identifier
  _ <- Token.symbol "="
  val <- expression
  end <- getPosition
  return $ AST.KeywordArgument keyword val (start, end)

positionalArgument :: Parser AST.Argument
positionalArgument = fmap AST.PositionalArgument expression

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
  statements <- many statement
  end <- getPosition
  eof >> return (AST.SourceFile statements (start, end))
