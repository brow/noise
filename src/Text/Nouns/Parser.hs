module Text.Nouns.Parser
( parse
, ParseError
) where

import Control.Applicative
import Text.ParserCombinators.Parsec (ParseError, sepBy1, eof, option)
import Text.Parsec.Prim (try, (<?>))
import Text.Parsec.Pos (initialPos)
import qualified Text.Parsec.Expr as Expr
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
colorLiteral = ranged (AST.ColorLiteral <$> Token.colorLiteral)

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

operator :: AST.Operator -> Parser AST.Operator
operator op = Token.reservedOp str >> return op
  where str = case op of
          AST.Add -> "+"
          AST.Sub -> "-"
          AST.Mul -> "*"
          AST.Div -> "/"

expression :: Parser AST.Expression
expression = Expr.buildExpressionParser opTable term
  where opTable = [ [infixLeft AST.Mul, infixLeft AST.Div]
                  , [infixLeft AST.Add, infixLeft AST.Sub] ]
        infixLeft op = Expr.Infix (AST.Operation <$> operator op) Expr.AssocLeft
        term = colorLiteral
           <|> floatLiteral
           <|> stringLiteral
           <|> functionCall
           <|> Token.parens expression
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
