module Text.Nouns.Parser.Token where

import qualified Text.Parsec.Token as T
import qualified Text.Nouns.Parser.Language as Language

T.TokenParser { T.identifier = identifier
              , T.parens = parens
              , T.integer = integer
              , T.commaSep = commaSeparated
              , T.whiteSpace = whiteSpace
              } = T.makeTokenParser Language.nounsDef
