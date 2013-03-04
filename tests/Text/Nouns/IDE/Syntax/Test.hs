{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-missing-signatures #-}

module Text.Nouns.IDE.Syntax.Test where

import Test.Framework
import Test.HUnit.Lang (Assertion)
import Text.Nouns.IDE.Syntax (Syntax(..), syntaxify)
import qualified Text.Nouns.Parser as Parser

{-# ANN module "HLint: ignore Use camelCase" #-}

assertSyntax :: Syntax -> String -> Assertion
assertSyntax hl str = case Parser.parse str of
  Left err -> assertFailure $ "parse failed: " ++ show err
  Right ast -> assertEqual hl $ syntaxify str ast

test_syntaxify_function = assertSyntax
  Syntax { argumentRanges = [ (9,1)
                            , (11,2)
                            ]
         , identifierRanges = [ (0,8) ]
         , functionCallRanges = [ (0,14) ]
         }
  "fun.tion(1,22)"

test_syntaxify_functions = assertSyntax
  Syntax { argumentRanges = [ (3, 1)
                            , (5, 2)
                            , (13, 3)
                            ]
         , identifierRanges = [ (0,2)
                              , (9,3)
                              ]
         , functionCallRanges = [ (0,9)
                                , (9,8)
                                ]
         }
  "fn(1,22) fn2(333)"
