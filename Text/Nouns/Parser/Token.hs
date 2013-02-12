module Text.Nouns.Parser.Token where

import qualified Text.Parsec.Token as T
import qualified Text.Nouns.Parser.Language as Language
import Text.ParserCombinators.Parsec

identifier :: Parser String
identifier = T.identifier tokenParser

integer :: Parser Integer
integer = T.integer tokenParser

whiteSpace :: Parser ()
whiteSpace = T.whiteSpace tokenParser

parens :: Parser a -> Parser a
parens = T.parens tokenParser

commaSeparated :: Parser a -> Parser [a]
commaSeparated = T.commaSep tokenParser

tokenParser :: T.TokenParser st
tokenParser = T.makeTokenParser Language.nounsDef
