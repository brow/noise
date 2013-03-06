{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-missing-signatures #-}

module Text.Nouns.Parser.Test where

import Test.Framework
import Test.HUnit.Lang (Assertion)
import qualified Text.Nouns.Parser as Parser
import qualified Text.Nouns.Parser.AST as AST

{-# ANN module "HLint: ignore Use camelCase" #-}

assertParseFnCall :: String -> AST.FunctionCall -> Assertion
assertParseFnCall str expected = case Parser.parse str of
  Right ast -> assertEqual (AST.SourceFile [expected] (AST.rangeInSource expected)) ast
  Left err -> assertFailure $ "parse failed: " ++ show err

arg :: Double -> (Int, Int) -> AST.Argument
arg val (loc,len) = AST.PositionalArgument (AST.Value val argRange) argRange
  where argRange = AST.oneLineRange "" loc len

range :: Int -> Int -> AST.SourceRange
range = AST.oneLineRange ""

test_function_no_args =
  assertParseFnCall
    "function()"
    (AST.FunctionCall
      (AST.QualifiedIdentifier ["function"] (range 1 8))
      []
      (range 1 10))

test_function_qualified =
  assertParseFnCall
    "shape.rectangle(0,100)"
    (AST.FunctionCall
      (AST.QualifiedIdentifier ["shape","rectangle"] (range 1 15))
      [ arg 0 (17,1)
      , arg 100 (19,3)
      ]
      (range 1 22))

test_function_keyword_arg =
  assertParseFnCall
    "fn(foo=123)"
    (AST.FunctionCall
      (AST.QualifiedIdentifier ["fn"] (range 1 2))
      [AST.KeywordArgument "foo" (AST.Value 123 (range 8 3)) (range 4 7)]
      (range 1 11))
