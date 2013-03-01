{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-missing-signatures #-}

module Text.Nouns.IDE.Syntax.Test where

import Test.Framework
import Test.HUnit.Lang (Assertion)
import qualified Text.Nouns.IDE.Syntax as S
import qualified Text.Nouns.Parser as Parser

{-# ANN module "HLint: ignore Use camelCase" #-}

assertHighlight :: S.Highlighting -> String -> Assertion
assertHighlight hl str = case Parser.parse str of
  Left err -> assertFailure $ "parse failed: " ++ show err
  Right ast -> assertEqual hl $ S.highlight str ast

test_highlight_function = assertHighlight
  S.Highlighting
    { S.argumentRanges = [ (9,1)
                         , (11,2)
                         ]
    }
  "function(1,22)"

test_highlight_functions = assertHighlight
  S.Highlighting
    { S.argumentRanges = [ (3, 1)
                         , (5, 2)
                         , (13, 3)
                         ]
    }
  "fn(1,22) fn2(333)"
