{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-missing-signatures #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Text.Nouns.Parser.Test where

import Test.Framework
import Assertion

{-# ANN module "HLint: ignore Use camelCase" #-}

test_unexpected_eof = assertError
  "Unexpected end of input. Expecting identifier, \"#\", float, integer, literal string or \")\"."
  "foo("
