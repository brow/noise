{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-missing-signatures #-}

module Text.Nouns.IDE.Syntax.Test where

import Test.Framework
import Test.HUnit.Lang (Assertion)
import Text.Nouns.Parser.AST (oneLineRange)
import qualified Text.Nouns.IDE.Syntax as S
import qualified Text.Nouns.Parser as Parser

{-# ANN module "HLint: ignore Use camelCase" #-}

assertHighlight :: S.Highlighting -> String -> Assertion
assertHighlight hl str = case Parser.parse str of
  Left err -> assertFailure $ "parse failed: " ++ show err
  Right ast -> assertEqual hl $ S.highlight ast

test_highlight_function = assertHighlight
  S.Highlighting
    { S.argumentRanges = [ oneLineRange "" 10 1
                         , oneLineRange "" 12 2
                         ]
    }
  "function(1,22)"

test_highlight_functions = assertHighlight
  S.Highlighting
    { S.argumentRanges = [ oneLineRange "" 4 1
                         , oneLineRange "" 6 2
                         , oneLineRange "" 14 3
                         ]
    }
  "fn(1,22) fn2(333)"
