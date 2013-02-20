module Text.Nouns
( process
) where

import qualified Text.Nouns.Parser as Parser
import qualified Text.Nouns.Compiler as Compiler
import           Text.Nouns.Renderer (render)

data Error = ParseError Parser.ParseError
           | CompileError Compiler.CompileError
           deriving Show

process :: String -> Either Error String
process src =
  case Parser.parse src of
    Left err -> Left (ParseError err)
    Right ast ->
      case Compiler.compile ast of
        Left err -> Left (CompileError err)
        Right document -> Right (render document)
