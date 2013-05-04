module Text.Nouns.Compiler.Document.Color
( Color(..)
, toHex
, fromHex
, toRGBHex
, alpha
, black
) where

import Data.Word
import Numeric (showHex, readHex)
import Control.Applicative

data Color = ARGB Word8 Word8 Word8 Word8
           | RGB Word8 Word8 Word8
           deriving (Eq, Show)

showHexByte :: Word8 -> String
showHexByte = pad . flip showHex ""
  where pad [x] = ['0', x]
        pad xs  = xs

readHexByte :: String -> Maybe Word8
readHexByte str@[_,_] = case readHex str of
  [(i,"")] -> Just i
  _        -> Nothing
readHexByte _ = Nothing

toHex :: Color -> String
toHex color = concatMap showHexByte $ case color of
  ARGB a r g b -> [a ,r, g, b]
  RGB r g b    -> [r, g, b]

toRGBHex :: Color -> String
toRGBHex color = toHex $ case color of
  ARGB _ r g b -> RGB r g b
  RGB {}       -> color

fromHex :: String -> Maybe Color
fromHex [a0,a1,r0,r1,g0,g1,b0,b1] = ARGB
  <$> readHexByte [a0,a1]
  <*> readHexByte [r0,r1]
  <*> readHexByte [g0,g1]
  <*> readHexByte [b0,b1]
fromHex [r0,r1,g0,g1,b0,b1] = RGB
  <$> readHexByte [r0,r1]
  <*> readHexByte [g0,g1]
  <*> readHexByte [b0,b1]
fromHex _ = Nothing

alpha :: Color -> Maybe Double
alpha (ARGB a _ _ _) = Just (fromIntegral a / 255.0)
alpha _              = Nothing

black :: Color
black = RGB 0 0 0
