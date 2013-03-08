module Text.Nouns.IDE.Completion
( parametersForString
, Parameters(..)
, Dictionary(..)
) where

import Text.ParserCombinators.Parsec (Parser)
import qualified Text.Parsec as Parsec
import qualified Text.Nouns.Parser as Parser
import qualified Text.Nouns.Parser.Token as Token

data Dictionary = FunctionCallDictionary deriving (Eq, Show)

data Parameters = Parameters Dictionary String deriving (Eq, Show)

data Frame = FunctionCallFrame | ArgumentListFrame

parametersForString :: String -> Parameters
parametersForString source = case Parsec.parse sourceFile "" source of
  Right substring -> substring
  Left _ -> Parameters FunctionCallDictionary source

functionCall :: CompletionParser
functionCall = do
  pushFrame FunctionCallFrame
  Parser.qualifiedIdentifier
  Token.parens argumentList
  popFrame

sourceFile :: CompletionParser
sourceFile = do
  Token.whiteSpace
  Parsec.many functionCall
  eof

anyString :: Parser String
anyString = Parsec.many Parsec.anyChar
