module Text.Nouns
( Output(..)
, Error
, Syntax.Highlighting(..)
, process
) where

import qualified Text.Nouns.Parser as Parser
import qualified Text.Nouns.Compiler as Compiler
import qualified Text.Nouns.IDE.Syntax as Syntax
import           Text.Nouns.Renderer (render)

data Error = ParseError Parser.ParseError
           | CompileError Compiler.CompileError
           deriving Show

data Output = Output
  { svg :: Maybe String
  , errors :: [Error]
  , highlighting :: Maybe Syntax.Highlighting }

process :: String -> Output
process src =
  case Parser.parse src of
    Left err -> Output
      { svg = Nothing
      , errors = [ParseError err]
      , highlighting = Nothing }
    Right ast ->
      let hl = Syntax.highlight src ast
      in case Compiler.compile ast of
        Left err -> Output
          { svg = Nothing
          , errors = [CompileError err]
          , highlighting = Just hl }
        Right document -> Output
          { svg = Just (render document)
          , errors = []
          , highlighting = Just hl }
