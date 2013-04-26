module Text.Nouns.Parser.Token
( Parser
, identifier
, number
, hexRGB
, stringLiteral
, whiteSpace
, parens
, commaSeparated
, dot
, symbol
, reserved
) where

import Control.Applicative
import Text.Parsec hiding (many, (<|>))
import qualified Text.Parsec.Token as Parsec.Token
import qualified Text.Nouns.Parser.Token.Internal as Internal
import qualified Text.Nouns.Parser.Language as Language
import qualified Text.Nouns.Parser.AST as AST

{-# ANN module "HLint: ignore Use string literal" #-}

type Parser = Parsec String SourcePos

markSourcePos :: Parser ()
markSourcePos = getPosition >>= setState

whiteSpace :: Parser ()
whiteSpace = Parsec.Token.whiteSpace tokenParser
  where tokenParser = Parsec.Token.makeTokenParser Language.def

lexeme :: Parser a -> Parser a
lexeme p = p <* markSourcePos <* whiteSpace

identifier :: Parser AST.Identifier
identifier = lexeme Internal.identifier

float :: Parser Double
float = lexeme Internal.float

integer :: Parser Integer
integer = lexeme Internal.integer

number :: Parser Double
number = try float
         <|> fmap fromInteger integer

hexRGB :: Parser String
hexRGB = lexeme (char '#' >> count 6 hexDigit)

stringLiteral :: Parser String
stringLiteral = lexeme Internal.stringLiteral

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

commaSeparated :: Parser a -> Parser [a]
commaSeparated p = sepBy p (symbol ",")

dot :: Parser String
dot = symbol "."

symbol :: String -> Parser String
symbol = lexeme . string

reserved :: String -> Parser ()
reserved name = lexeme $ Internal.reserved name
