module Text.Nouns.Parser.Token 
( identifier
, number
, whiteSpace
, parens
, commaSeparated
) where

import qualified Text.Parsec.Token as T
import qualified Text.Nouns.Parser.Language as Language
import Text.ParserCombinators.Parsec

identifier :: Parser String
identifier = T.identifier tokenParser

float :: Parser Double
float = T.float tokenParser

integer :: Parser Integer
integer = T.integer tokenParser

number :: Parser Double
number = lexeme (try float <|> fmap fromInteger integer)

whiteSpace :: Parser ()
whiteSpace = T.whiteSpace tokenParser

parens :: Parser a -> Parser a
parens = T.parens tokenParser

commaSeparated :: Parser a -> Parser [a]
commaSeparated = T.commaSep tokenParser

lexeme :: Parser a -> Parser a
lexeme = T.lexeme tokenParser

tokenParser :: T.TokenParser st
tokenParser = T.makeTokenParser Language.nounsDef
