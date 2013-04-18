module Text.Nouns.Parser.Language (nounsDef) where

import Text.Parsec
import Text.Parsec.Token as Token

nounsDef :: Token.LanguageDef st
nounsDef = Token.LanguageDef
  { commentStart = "/*"
  , commentEnd = "*/"
  , commentLine = "//"
  , nestedComments = True
  , identStart = letter <|> char '_'
  , identLetter = alphaNum <|> char '_'
  , opStart = parserZero
  , opLetter = parserZero
  , reservedNames = ["let"]
  , reservedOpNames = []
  , caseSensitive = True }
