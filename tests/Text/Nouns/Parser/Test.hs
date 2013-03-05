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

test_function_no_args =
  assertParseFnCall
    "function()"
    (AST.FunctionCall
      (AST.QualifiedIdentifier ["function"] (AST.oneLineRange "" 1 8))
      []
      (AST.oneLineRange "" 1 10))

test_function_qualified =
  assertParseFnCall
    "shape.rectangle(0,0,100,200)"
    (AST.FunctionCall
      (AST.QualifiedIdentifier ["shape","rectangle"] (AST.oneLineRange "" 1 15))
      [ AST.Argument 0 (AST.oneLineRange "" 17 1)
      , AST.Argument 0 (AST.oneLineRange "" 19 1)
      , AST.Argument 100 (AST.oneLineRange "" 21 3)
      , AST.Argument 200 (AST.oneLineRange "" 25 3)
      ]
      (AST.oneLineRange "" 1 28))
