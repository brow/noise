module Token where

import qualified Text.Parsec.Token as T
import qualified Language

T.TokenParser { T.parens = parens
              } = T.makeTokenParser Language.nounsDef
