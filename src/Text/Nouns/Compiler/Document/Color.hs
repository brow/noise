module Text.Nouns.Compiler.Document.Color
( Color
, toHex
, fromHex
, black
) where

import Numeric (showHex, readHex)
import Control.Applicative

data Color = ARGB Int Int Int Int
           | RGB Int Int Int
           deriving (Eq)

instance Show Color where
  show = ('#':) . toHex

showHexByte :: Int -> String
showHexByte = pad . flip showHex "" . (`mod` 256)
  where pad [x] = ['0', x]
        pad xs  = xs

readHexByte :: String -> Maybe Int
readHexByte str@[_,_] = case readHex str of
  [(i,"")] -> Just i
  _        -> Nothing
readHexByte _ = Nothing

toHex :: Color -> String
toHex color = concatMap showHexByte $ case color of
  ARGB a r g b -> [a ,r, g, b]
  RGB r g b    -> [r, g, b]

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

black :: Color
black = RGB 0 0 0
