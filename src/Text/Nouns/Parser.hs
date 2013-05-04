module Text.Nouns.Parser
( parse
, ParseError
) where

import Control.Applicative
import Text.ParserCombinators.Parsec (ParseError, sepBy1, eof, option)
import Text.Parsec.Prim (try, (<?>))
import Text.Parsec.Pos (initialPos)
import qualified Text.Parsec.Prim (runParser)
import Text.Nouns.Parser.Token (Parser, ranged)
import qualified Text.Nouns.Parser.Token as Token
import qualified Text.Nouns.Parser.AST as AST

parse :: String -> Either ParseError AST.SourceFile
parse = Text.Parsec.Prim.runParser sourceFile (initialPos "" ) ""

qualifiedIdentifier :: Parser AST.QualifiedIdentifier
qualifiedIdentifier = ranged
  (AST.QualifiedIdentifier <$> sepBy1 Token.identifier Token.dot)

floatLiteral :: Parser AST.Expression
floatLiteral = ranged (AST.FloatLiteral <$> Token.number)

colorLiteral :: Parser AST.Expression
colorLiteral = ranged $ fmap AST.ColorLiteral $
  try (Token.hexLiteral 8) <|> Token.hexLiteral 6

stringLiteral :: Parser AST.Expression
stringLiteral = ranged (AST.StringLiteral <$> Token.stringLiteral)

functionCall :: Parser AST.Expression
functionCall = ranged $ AST.FunctionCall
  <$> qualifiedIdentifier
  <*> option [] (Token.parens (Token.commaSeparated argument))
  <*> option Nothing (Just <$> block)

block :: Parser AST.Block
block = ranged $ AST.Block
  <$> reserved "with"
  <*> many statement
  <*> reserved "end"

expression :: Parser AST.Expression
expression = colorLiteral
         <|> floatLiteral
         <|> stringLiteral
         <|> functionCall
         <?> "expression"

expressionStatement :: Parser AST.Statement
expressionStatement = AST.ExpressionStatement <$> expression

argumentPrototype :: Parser AST.ArgumentPrototype
argumentPrototype = ranged (AST.RequiredArgumentPrototype <$> Token.identifier)

functionPrototype :: Parser AST.FunctionPrototype
functionPrototype = ranged $ AST.FunctionPrototype
  <$> qualifiedIdentifier
  <*> option [] (Token.parens (Token.commaSeparated argumentPrototype))

reserved :: String -> Parser AST.Reserved
reserved str = ranged $ Token.reserved str >> return (AST.Reserved str)

functionDefStatement :: Parser AST.Statement
functionDefStatement = ranged $ AST.DefinitionStatement
  <$> reserved "let"
  <*> functionPrototype <* Token.symbol "="
  <*> expression

statement :: Parser AST.Statement
statement = functionDefStatement
        <|> expressionStatement
        <?> "statement"

keywordArgument :: Parser AST.Argument
keywordArgument = ranged $ AST.KeywordArgument
  <$> try (Token.identifier <* Token.symbol ":")
  <*> expression

positionalArgument :: Parser AST.Argument
positionalArgument = AST.PositionalArgument <$> expression

argument :: Parser AST.Argument
argument = keywordArgument
       <|> positionalArgument
       <?> "argument"

sourceFile :: Parser AST.SourceFile
sourceFile = do
  Token.whiteSpace
  ast <- ranged (AST.SourceFile <$> many statement)
  eof
  return ast
