{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-missing-signatures #-}
module Text.Nouns.Parser.Test where

import Test.Framework
import Test.HUnit.Lang (Assertion)
import qualified Text.Nouns.Error as Error
import qualified Text.Nouns.Parser as Parser
import qualified Text.Nouns.Parser.AST as AST
import Text.Nouns.SourceRange (SourceRange, rangeInSource, oneLineRange)

{-# ANN module "HLint: ignore Use camelCase" #-}
