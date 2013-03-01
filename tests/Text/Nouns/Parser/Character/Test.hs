{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-missing-signatures #-}

module Text.Nouns.Parser.Character.Test where

import Test.Framework
import qualified Text.Parsec.Pos as Pos
import qualified Text.Nouns.Parser.Character as Character

{-# ANN module "HLint: ignore Use camelCase" #-}

data SourceAndLocation = SourceAndLocation String Character.Location deriving (Show)

instance Arbitrary SourceAndLocation where
  arbitrary = do
    source <- fmap getNonEmpty arbitrary
    location <- choose (0, length source - 1)
    return $ SourceAndLocation source location

test_location_at = assertEqual (Just 1) $
  Character.locationAt "abc" (Pos.newPos "" 1 2)

test_range_at = assertEqual (Just (1,1)) $
  Character.rangeAt "abc" (Pos.newPos "" 1 2, Pos.newPos "" 1 3)

prop_location_zero :: String -> Bool
prop_location_zero src = Character.locationAt src (Pos.initialPos "") == Just 0

prop_location :: String -> SourceAndLocation -> Property
prop_location name (SourceAndLocation source loc) =
  not (null source) ==> Character.locationAt source pos == Just loc
  where pos = Pos.updatePosString (Pos.initialPos name) $ take loc source
