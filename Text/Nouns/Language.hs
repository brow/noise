module Text.Nouns.Language (nounsDef) where

import Text.Parsec
import Text.Parsec.Token

nounsDef :: LanguageDef st
nounsDef = LanguageDef
  { commentStart = "/*"
  , commentEnd = "*/"
  , commentLine = "//"
  , nestedComments = True
  , identStart = letter <|> char '_'
  , identLetter = alphaNum <|> char '_'
  , opStart = parserZero
  , opLetter = parserZero
  , reservedNames = []
  , reservedOpNames = []
  , caseSensitive = True }
