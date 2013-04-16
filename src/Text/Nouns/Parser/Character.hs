module Text.Nouns.Parser.Character
( Location
, Length
, Range
, locationAt
, rangeAt
) where

import Text.Parsec.Pos (SourcePos, updatePosChar, initialPos, sourceName)
import Text.Nouns.SourceRange (SourceRange)
import Data.List (elemIndex)

type Location = Int

type Length = Int

type Range = (Location, Length)

locationAt :: String -> SourcePos -> Maybe Location
locationAt source pos = elemIndex pos positions
  where positions = scanl updatePosChar firstPos source
        firstPos = initialPos (sourceName pos)

rangeAt :: String -> SourceRange -> Maybe Range
rangeAt source (fromPos, toPos) = do
  fromLoc <- locationAt source fromPos
  toLoc <- locationAt source toPos
  return (fromLoc, toLoc - fromLoc)
