{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}

module Text.Nouns.Parser.Token.Internal
( identifier
, natFloat
, stringLiteral
, reserved
, reservedOp
) where

import Data.Char (isAlpha, toLower, toUpper, digitToInt)
import Data.List (sort)
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Token
  ( caseSensitive
  , reservedNames
  , identStart
  , identLetter
  , opLetter
  )
import qualified Text.Nouns.Parser.Language

{-# ANN module "HLint: ignore" #-}

languageDef = Text.Nouns.Parser.Language.def

identifier =
    try $
    do{ name <- ident
      ; if (isReservedName name)
         then unexpected ("reserved word " ++ show name)
         else return name
      }

ident
    = do{ c <- identStart languageDef
        ; cs <- many (identLetter languageDef)
        ; return (c:cs)
        }
    <?> "identifier"

isReservedName name
    = isReserved theReservedNames caseName
    where
      caseName      | caseSensitive languageDef  = name
                    | otherwise               = map toLower name

isReserved names name
    = scan names
    where
      scan []       = False
      scan (r:rs)   = case (compare r name) of
                        LT  -> scan rs
                        EQ  -> True
                        GT  -> False

theReservedNames
    | caseSensitive languageDef  = sort reserved
    | otherwise                  = sort . map (map toLower) $ reserved
    where
      reserved = reservedNames languageDef

natFloat        = do{ char '0'
                    ; zeroNumFloat
                    }
                  <|> decimalFloat

zeroNumFloat    =  do{ n <- hexadecimal <|> octal
                     ; return (Left n)
                     }
                <|> decimalFloat
                <|> fractFloat 0
                <|> return (Left 0)

decimalFloat    = do{ n <- decimal
                    ; option (Left n)
                             (fractFloat n)
                    }

fractFloat n    = do{ f <- fractExponent n
                    ; return (Right f)
                    }

fractExponent n = do{ fract <- fraction
                    ; expo  <- option 1.0 exponent'
                    ; return ((fromInteger n + fract)*expo)
                    }
                <|>
                  do{ expo <- exponent'
                    ; return ((fromInteger n)*expo)
                    }

fraction        = do{ char '.'
                    ; digits <- many1 digit <?> "fraction"
                    ; return (foldr op 0.0 digits)
                    }
                  <?> "fraction"
                where
                  op d f    = (f + fromIntegral (digitToInt d))/10.0

exponent'       = do{ oneOf "eE"
                    ; f <- sign
                    ; e <- decimal <?> "exponent"
                    ; return (power (f e))
                    }
                  <?> "exponent"
                where
                   power e  | e < 0      = 1.0/power(-e)
                            | otherwise  = fromInteger (10^e)

sign            =   (char '-' >> return negate)
                <|> (char '+' >> return id)
                <|> return id

decimal         = number 10 digit
hexadecimal     = do{ oneOf "xX"; number 16 hexDigit }
octal           = do{ oneOf "oO"; number 8 octDigit  }

number base baseDigit
    = do{ digits <- many1 baseDigit
        ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
        ; seq n (return n)
        }

stringLiteral   = do{ str <- between (char '"')
                                     (char '"' <?> "end of string")
                                     (many stringChar)
                    ; return (foldr (maybe id (:)) "" str)
                    }
                  <?> "literal string"

stringChar      =   do{ c <- stringLetter; return (Just c) }
                <|> stringEscape
                <?> "string character"

stringLetter    = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

stringEscape    = do{ char '\\'
                    ;     do{ escapeGap  ; return Nothing }
                      <|> do{ escapeEmpty; return Nothing }
                      <|> do{ esc <- escapeCode; return (Just esc) }
                    }

escapeEmpty     = char '&'

escapeGap       = do{ many1 space
                    ; char '\\' <?> "end of string gap"
                    }

escapeCode      = charEsc <|> charNum <|> charAscii <|> charControl
                <?> "escape code"

charControl     = do{ char '^'
                    ; code <- upper
                    ; return (toEnum (fromEnum code - fromEnum 'A'))
                    }

charNum         = do{ code <- decimal
                              <|> do{ char 'o'; number 8 octDigit }
                              <|> do{ char 'x'; number 16 hexDigit }
                    ; return (toEnum (fromInteger code))
                    }

charEsc         = choice (map parseEsc escMap)
                where
                  parseEsc (c,code)     = do{ char c; return code }

charAscii       = choice (map parseAscii asciiMap)
                where
                  parseAscii (asc,code) = try (do{ string asc; return code })


escMap          = zip "abfnrtv\\\"\'" "\a\b\f\n\r\t\v\\\"\'"
asciiMap        = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)

ascii2codes     = ["BS","HT","LF","VT","FF","CR","SO","SI","EM",
                   "FS","GS","RS","US","SP"]
ascii3codes     = ["NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL",
                   "DLE","DC1","DC2","DC3","DC4","NAK","SYN","ETB",
                   "CAN","SUB","ESC","DEL"]

ascii2          = ['\BS','\HT','\LF','\VT','\FF','\CR','\SO','\SI',
                   '\EM','\FS','\GS','\RS','\US','\SP']
ascii3          = ['\NUL','\SOH','\STX','\ETX','\EOT','\ENQ','\ACK',
                   '\BEL','\DLE','\DC1','\DC2','\DC3','\DC4','\NAK',
                   '\SYN','\ETB','\CAN','\SUB','\ESC','\DEL']

reserved name =
    try $
    do{ caseString name
      ; notFollowedBy (identLetter languageDef) <?> ("end of " ++ show name)
      }

caseString name
    | caseSensitive languageDef  = string name
    | otherwise               = do{ walk name; return name }
    where
      walk []     = return ()
      walk (c:cs) = do{ caseChar c <?> msg; walk cs }

      caseChar c  | isAlpha c  = char (toLower c) <|> char (toUpper c)
                  | otherwise  = char c

      msg         = show name

reservedOp name =
    try $
    do{ string name
      ; notFollowedBy (opLetter languageDef) <?> ("end of " ++ show name)
      }
