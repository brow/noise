{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-missing-signatures #-}

module Text.Nouns.Parser.Character.Test where

import Test.Framework
import Text.Parsec.Pos as Pos
import Text.Nouns.Parser.Character as Character

{-# ANN module "HLint: ignore Use camelCase" #-}

data SourceAndLocation = SourceAndLocation String Location deriving (Show)

instance Arbitrary SourceAndLocation where
  arbitrary = do
    source <- fmap getNonEmpty arbitrary
    location <- choose (0, length source - 1)
    return $ SourceAndLocation source location

prop_initial_pos :: String -> Bool
prop_initial_pos src = Character.locationAt src (Pos.initialPos "") == Just 0

prop_correct :: String -> SourceAndLocation -> Property
prop_correct name (SourceAndLocation source loc) =
  not (null source) ==> locationAt source pos == Just loc
  where pos = Pos.updatePosString (initialPos name) $ take loc source
