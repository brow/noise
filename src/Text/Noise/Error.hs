module Text.Noise.Error
( Error(..)
) where

import qualified Data.List as List
import qualified Text.Noise.SourceRange as SourceRange
import qualified Text.Parsec.Error as Parsec

class (SourceRange.HasSourceRange a) => Error a where
  message :: a -> String

instance Error Parsec.ParseError where
  message = showErrorMessages . Parsec.errorMessages

showErrorMessages :: [Parsec.Message] -> String
showErrorMessages msgs
    | null msgs = msgUnknown
    | otherwise = unwords $ map (++ ".") $ clean
                 [showSysUnExpect,showUnExpect,showExpect,showMessages]
    where
      (sysUnExpect, msgs1) = span (Parsec.SysUnExpect "" ==) msgs
      (unExpect, msgs2)    = span (Parsec.UnExpect    "" ==) msgs1
      (expect, messages)   = span (Parsec.Expect      "" ==) msgs2

      showExpect      = showMany msgExpecting expect
      showUnExpect    = showMany msgUnExpected unExpect
      showSysUnExpect | not (null unExpect) ||
                        null sysUnExpect = ""
                      | null firstMsg    = msgUnExpected ++ " " ++ msgEndOfInput
                      | otherwise        = msgUnExpected ++ " " ++ firstMsg
        where
          firstMsg  = Parsec.messageString (head sysUnExpect)

      showMessages      = showMany "" messages
      showMany pre msgs' = case clean (map Parsec.messageString msgs') of
                            [] -> ""
                            ms | null pre  -> commasOr ms
                               | otherwise -> pre ++ " " ++ commasOr ms

      commasOr []       = ""
      commasOr [m]      = m
      commasOr ms       = commaSep (init ms) ++ " " ++ msgOr ++ " " ++ last ms
      commaSep          = seperate ", " . clean

      seperate   _ []     = ""
      seperate   _ [m]    = m
      seperate sep (m:ms) = m ++ sep ++ seperate sep ms

      clean             = List.nub . filter (not . null)

      msgOr = "or"
      msgUnknown = "Parse error"
      msgExpecting = "Expecting"
      msgUnExpected = "Unexpected"
      msgEndOfInput = "end of input"
