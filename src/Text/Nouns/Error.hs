module Text.Nouns.Error
( Error(..)
) where

import Text.Nouns.SourceRange as SourceRange
import Text.Parsec.Error as Parsec

class (SourceRange.HasSourceRange a) => Error a where
  message :: a -> String

instance Error Parsec.ParseError where
  message = showMessages . Parsec.errorMessages
    where showMessages = Parsec.showErrorMessages
            "or"
            "unknown parse error"
            "expecting"
            "unexpected"
            "end of input"
