module Text.Noise.Parser.Token
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
, reservedOp
) where

import Control.Monad
import Control.Applicative
import Text.Parsec hiding (many, (<|>))
import qualified Text.Parsec.Token as Parsec.Token
import Text.Noise.SourceRange (SourceRange)
import qualified Text.Noise.Parser.Token.Internal as Internal
import qualified Text.Noise.Parser.Language as Language
import qualified Text.Noise.Parser.AST as AST

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

number :: Parser Double
number = lexeme $ do
  naturalOrFloat <- Internal.natFloat
  return $ case naturalOrFloat of
    Left integer -> fromInteger integer
    Right double -> double

colorLiteral :: Parser String
colorLiteral = lexeme (char '#' >> colorStr)
  where colorStr = try hexColorStr <?> "hex color of form RRGGBB or AARRGGBB"
        hexColorStr = do
          str <- many hexDigit
          unless (length str `elem` [6,8]) (unexpected "color format")
          return str

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
reserved = lexeme . Internal.reserved

reservedOp :: String -> Parser ()
reservedOp = lexeme . Internal.reservedOp
