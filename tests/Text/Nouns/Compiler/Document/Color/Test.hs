{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-missing-signatures #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Text.Nouns.Compiler.Document.Color.Test where

import Test.Framework
import Control.Applicative
import Data.Maybe (fromJust, isNothing, isJust)
import qualified Text.Nouns.Compiler.Document.Color as Color

{-# ANN module "HLint: ignore Use camelCase" #-}

newtype HexColorString = HexColorString String deriving (Show)

instance Arbitrary HexColorString where
  arbitrary = do
    len <- elements [6, 8]
    let hexDigits = elements "0123456789abcdef"
    HexColorString <$> vectorOf len hexDigits

prop_toHex_is_inverse_of_fromHex :: HexColorString -> Bool
prop_toHex_is_inverse_of_fromHex (HexColorString hexStr) =
  Just hexStr == (Color.toHex <$> Color.fromHex hexStr)

prop_toRGBHex_eq_toHex_over_RGB :: HexColorString -> Property
prop_toRGBHex_eq_toHex_over_RGB (HexColorString str) =
  (length str == 6) ==> Color.toRGBHex color == Color.toHex color
  where color = fromJust (Color.fromHex str)

prop_alpha_is_Nothing_over_RGB :: HexColorString -> Bool
prop_alpha_is_Nothing_over_RGB (HexColorString str)
  | length str == 6 = isNothing alpha
  | length str == 8 = isJust alpha
  | otherwise       = False
  where alpha = Color.fromHex str >>= Color.alpha
