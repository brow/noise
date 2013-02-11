module Language.Nouns.Core (document) where

import Text.ParserCombinators.Parsec
import qualified Language.Nouns.Token as Token

data FunctionCall = FunctionCall FunctionName [Argument] deriving (Show)
type FunctionName = String
type Argument = Integer

functionCall :: Parser FunctionCall
functionCall = do
  name <- Token.identifier
  args <- Token.parens (Token.commaSeparated Token.integer)
  return $ FunctionCall name args

document :: Parser [FunctionCall]
document = do
  Token.whiteSpace
  calls <- many functionCall
  eof >> return calls
