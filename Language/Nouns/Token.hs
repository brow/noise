module Language.Nouns.Token where

import qualified Text.Parsec.Token as T
import qualified Language.Nouns.Language as Language

T.TokenParser { T.identifier = identifier
              , T.parens = parens
              , T.integer = integer
              , T.commaSep = commaSeparated
              , T.whiteSpace = whiteSpace
              } = T.makeTokenParser Language.nounsDef
