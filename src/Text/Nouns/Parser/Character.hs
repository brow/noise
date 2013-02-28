module Text.Nouns.Parser.Character
( Location
, Length
, Range
, locationAt
) where

import Text.Parsec.Pos (SourcePos, updatePosChar, initialPos, sourceName)
import Data.List (elemIndex)

type Location = Int

type Length = Int

type Range = (Location, Length)

locationAt :: String -> SourcePos -> Maybe Location
locationAt source pos = elemIndex pos positions
  where positions = scanl updatePosChar firstPos source
        firstPos = initialPos (sourceName pos)

