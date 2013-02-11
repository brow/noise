module Text.Nouns.Core (document) where

import Text.ParserCombinators.Parsec
import qualified Text.Nouns.Token as Token

data Document = Document [FunctionCall] deriving Show
data FunctionCall = FunctionCall FunctionName [Argument] deriving Show
type FunctionName = String
type Argument = Integer

functionCall :: Parser FunctionCall
functionCall = do
  name <- Token.identifier
  args <- Token.parens (Token.commaSeparated Token.integer)
  return $ FunctionCall name args

document :: Parser Document
document = do
  Token.whiteSpace
  calls <- many functionCall
  eof >> return (Document calls)
