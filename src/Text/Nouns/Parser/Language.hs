module Text.Nouns.Parser.Language
( def
) where

import Text.Parsec
import Text.Parsec.Token as Parsec.Token

def :: Parsec.Token.LanguageDef st
def = Parsec.Token.LanguageDef
  { commentStart = "/*"
  , commentEnd = "*/"
  , commentLine = "//"
  , nestedComments = True
  , identStart = letter <|> char '_'
  , identLetter = alphaNum <|> char '_'
  , opStart = parserZero
  , opLetter = parserZero
  , reservedNames = ["let", "with", "end"]
  , reservedOpNames = []
  , caseSensitive = True }
