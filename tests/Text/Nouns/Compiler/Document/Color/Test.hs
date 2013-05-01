{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-missing-signatures #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Text.Nouns.Compiler.Document.Color.Test where

import Test.Framework
import Control.Applicative
import qualified Text.Nouns.Compiler.Document.Color as Color

{-# ANN module "HLint: ignore Use camelCase" #-}

newtype HexColorString = HexColorString String deriving (Show)

instance Arbitrary HexColorString where
  arbitrary = HexColorString <$> vectorOf 6 (elements "0123456789abcdef")

prop_toHex_is_inverse_of_fromHex :: HexColorString -> Bool
prop_toHex_is_inverse_of_fromHex (HexColorString hexStr) =
  Just hexStr == (Color.toHex <$> Color.fromHex hexStr)
