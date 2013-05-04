module Text.Nouns.Parser.Token
( Parser
, ranged
, identifier
, number
, colorLiteral
, stringLiteral
, whiteSpace
, parens
, commaSeparated
, dot
, symbol
, reserved
) where

import Control.Monad
import Control.Applicative
import Text.Parsec hiding (many, (<|>))
import qualified Text.Parsec.Token as Parsec.Token
import Text.Nouns.SourceRange (SourceRange)
import qualified Text.Nouns.Parser.Token.Internal as Internal
import qualified Text.Nouns.Parser.Language as Language
import qualified Text.Nouns.Parser.AST as AST

{-# ANN module "HLint: ignore Use string literal" #-}

type Parser = Parsec String SourcePos

markPosition :: Parser ()
markPosition = getPosition >>= setState

getMarkedPosition :: Parser SourcePos
getMarkedPosition = getState

ranged :: Parser (SourceRange -> a) -> Parser a
ranged p = do
  start <- getPosition
  x <- p
  end <- getMarkedPosition
  return $ x (start, end)

whiteSpace :: Parser ()
whiteSpace = Parsec.Token.whiteSpace tokenParser
  where tokenParser = Parsec.Token.makeTokenParser Language.def

lexeme :: Parser a -> Parser a
lexeme p = p <* markPosition <* whiteSpace

identifier :: Parser AST.Identifier
identifier = lexeme Internal.identifier

float :: Parser Double
float = lexeme Internal.float

integer :: Parser Integer
integer = lexeme Internal.integer

number :: Parser Double
number = try float
         <|> fmap fromInteger integer

colorLiteral :: Parser String
colorLiteral = lexeme $ do
  char '#'
  (try $ do
    str <- many hexDigit
    unless
      (length str `elem` [6,8])
      (unexpected "color format")
    return str) <?> "form RRGGBB or AARRGGBB"

stringLiteral :: Parser String
stringLiteral = lexeme Internal.stringLiteral

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

commaSeparated :: Parser a -> Parser [a]
commaSeparated p = sepEndBy p (symbol ",")

dot :: Parser String
dot = symbol "."

symbol :: String -> Parser String
symbol = lexeme . string

reserved :: String -> Parser ()
reserved name = lexeme $ Internal.reserved name
