{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-missing-signatures #-}
module Text.Nouns.Parser.Test where

import Test.Framework
import Test.HUnit.Lang (Assertion)
import qualified Text.Nouns.Error as Error
import qualified Text.Nouns.Parser as Parser
import qualified Text.Nouns.Parser.AST as AST
import Text.Nouns.SourceRange (SourceRange, rangeInSource, oneLineRange)

{-# ANN module "HLint: ignore Use camelCase" #-}

assertParseFnCall :: String -> AST.FunctionCall -> Assertion
assertParseFnCall source expectedAST = case Parser.parse source of
  Right ast -> assertEqual (wrapFnCall expectedAST) ast
  Left err -> assertFailure $ "parse failed: " ++ show err
  where wrapFnCall fnCall = AST.SourceFile
          [AST.FunctionCallStatement fnCall]
          (rangeInSource fnCall)

assertParseErrorMessage :: String -> String -> Assertion
assertParseErrorMessage source expectedMessage = case Parser.parse source of
  Right _ -> assertFailure "parse succeeded"
  Left err -> assertEqual expectedMessage (Error.message err)

arg :: Double -> (Int, Int) -> AST.Argument
arg val (loc,len) = AST.PositionalArgument $ AST.FloatLiteral val argRange
  where argRange = oneLineRange "" loc len

range :: Int -> Int -> SourceRange
range = oneLineRange ""

test_missing_parens =
  assertParseErrorMessage
    "foo"
    "Unexpected end of input. Expecting letter or digit, \"_\", \".\" or \"(\"."

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

test_function_color_arg =
  assertParseFnCall
    "fill(#12aC3b)"
    (AST.FunctionCall
      (AST.QualifiedIdentifier ["fill"] (range 1 4))
      [AST.PositionalArgument $ AST.HexRGBLiteral "12aC3b" (range 6 7)]
      (range 1 13))

test_function_string_args =
  assertParseFnCall
    "text(\"def\")"
    (AST.FunctionCall
      (AST.QualifiedIdentifier ["text"] (range 1 4))
      [AST.PositionalArgument $ AST.StringLiteral "def" (range 6 5)]
      (range 1 11))

test_function_keyword_arg =
  assertParseFnCall
    "fn(foo:123)"
    (AST.FunctionCall
      (AST.QualifiedIdentifier ["fn"] (range 1 2))
      [AST.KeywordArgument "foo" (AST.FloatLiteral 123 (range 8 3)) (range 4 7)]
      (range 1 11))

test_function_fn_arg =
  assertParseFnCall
    "foo(bar())"
    (AST.FunctionCall
      (AST.QualifiedIdentifier ["foo"] (range 1 3))
      [AST.PositionalArgument
        (AST.FunctionCallExp
          (AST.FunctionCall
            (AST.QualifiedIdentifier ["bar"] (range 5 3))
            []
            (range 5 5)))]
      (range 1 10))
