module Text.Nouns.Parser
( parse
, ParseError
) where

import Control.Applicative
import Text.ParserCombinators.Parsec (ParseError, sepBy1, eof, option)
import Text.Parsec.Prim (getPosition, try, (<?>))
import Text.Parsec.Pos (initialPos)
import qualified Text.Parsec.Prim
import Text.Nouns.Parser.Token (Parser)
import qualified Text.Nouns.Parser.Token as Token
import qualified Text.Nouns.Parser.AST as AST

parse :: String -> Either ParseError AST.SourceFile
parse = Text.Parsec.Prim.runParser sourceFile (initialPos "" ) ""

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

stringLiteral :: Parser AST.Expression
stringLiteral = do
  start <- getPosition
  string <- Token.stringLiteral
  end <- getPosition
  return $ AST.StringLiteral string (start, end)

functionCall :: Parser AST.Expression
functionCall = do
  start <- getPosition
  name <- qualifiedIdentifier
  args <- option [] $ Token.parens (Token.commaSeparated argument)
  end <- getPosition
  return $ AST.FunctionCall name args (start, end)

expression :: Parser AST.Expression
expression = try hexRGBLiteral <|>
             try floatLiteral <|>
             try stringLiteral <|>
             functionCall

expressionStatement :: Parser AST.Statement
expressionStatement = AST.ExpressionStatement <$> expression

argumentPrototype :: Parser AST.ArgumentPrototype
argumentPrototype = do
  start <- getPosition
  identifier <- Token.identifier
  end <- getPosition
  return $ AST.RequiredArgumentPrototype identifier (start, end)

functionPrototype :: Parser AST.FunctionPrototype
functionPrototype = do
  start <- getPosition
  identifier <- qualifiedIdentifier
  args <- option [] $ Token.parens (Token.commaSeparated argumentPrototype)
  end <- getPosition
  return $ AST.FunctionPrototype identifier args (start, end)

reserved :: String -> Parser AST.Reserved
reserved str = do
  start <- getPosition
  Token.reserved str
  end <- getPosition
  return $ AST.Reserved str (start, end)

functionDefStatement :: Parser AST.Statement
functionDefStatement = do
  start <- getPosition
  reservedLet <- reserved "let"
  prototype <- functionPrototype
  _ <- Token.symbol "="
  definition <- expression
  end <- getPosition
  return $ AST.DefinitionStatement reservedLet prototype definition (start, end)

statement :: Parser AST.Statement
statement = try functionDefStatement <|>
            expressionStatement

keywordArgument :: Parser AST.Argument
keywordArgument = do
  start <- getPosition
  keyword <- Token.identifier
  _ <- Token.symbol ":"
  val <- expression
  end <- getPosition
  return $ AST.KeywordArgument keyword val (start, end)

positionalArgument :: Parser AST.Argument
positionalArgument = AST.PositionalArgument <$> expression

argument :: Parser AST.Argument
argument = try keywordArgument
           <|> positionalArgument
           <?> "argument"

sourceFile :: Parser AST.SourceFile
sourceFile = do
  start <- getPosition
  Token.whiteSpace
  statements <- many statement
  end <- getPosition
  eof >> return (AST.SourceFile statements (start, end))
