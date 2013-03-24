module Text.Nouns.Parser.Token
( identifier
, number
, hexRGB
, stringLiteral
, whiteSpace
, parens
, commaSeparated
, dot
, symbol
) where

import qualified Text.Parsec.Token as T
import qualified Text.Nouns.Parser.Language as Language
import qualified Text.Nouns.Parser.AST as AST
import Text.ParserCombinators.Parsec

identifier :: Parser AST.Identifier
identifier = T.identifier tokenParser

float :: Parser Double
float = T.float tokenParser

integer :: Parser Integer
integer = T.integer tokenParser

number :: Parser Double
number = lexeme (try float <|> fmap fromInteger integer)

hexRGB :: Parser String
hexRGB = lexeme $ do
  _ <- char '#'
  count 6 hexDigit

stringLiteral :: Parser String
stringLiteral = T.stringLiteral tokenParser

whiteSpace :: Parser ()
whiteSpace = T.whiteSpace tokenParser

parens :: Parser a -> Parser a
parens = T.parens tokenParser

commaSeparated :: Parser a -> Parser [a]
commaSeparated = T.commaSep tokenParser

dot :: Parser String
dot = T.dot tokenParser

symbol :: String -> Parser String
symbol = T.symbol tokenParser

lexeme :: Parser a -> Parser a
lexeme = T.lexeme tokenParser

tokenParser :: T.TokenParser st
tokenParser = T.makeTokenParser Language.nounsDef
