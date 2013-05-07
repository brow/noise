module Text.Nouns.SourceRange
( SourceRange
, HasSourceRange(..)
, oneLineRange
, zeroRange
, sourceName
, startLine
, startColumn
) where

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Pos as Parsec.Pos

type SourceRange = (Parsec.SourcePos, Parsec.SourcePos)

oneLineRange :: Parsec.SourceName -> Int -> Int -> SourceRange
oneLineRange name col len = ( Parsec.Pos.newPos name 1 col
                            , Parsec.Pos.newPos name 1 (col + len))

zeroRange :: SourceRange
zeroRange = oneLineRange "" 1 0

sourceName :: SourceRange -> String
sourceName = Parsec.Pos.sourceName . fst

startLine :: SourceRange -> Parsec.Pos.Line
startLine = Parsec.Pos.sourceLine . fst

startColumn :: SourceRange -> Parsec.Pos.Column
startColumn = Parsec.Pos.sourceColumn . fst

class HasSourceRange a where
  rangeInSource :: a -> SourceRange

instance HasSourceRange Parsec.ParseError where
  rangeInSource err = (pos, pos) where pos = Parsec.errorPos err
