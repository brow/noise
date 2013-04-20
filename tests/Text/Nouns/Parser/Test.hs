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
assertParseFnCall source fnCallAST = assertParse source $
  AST.SourceFile
    [AST.FunctionCallStatement fnCallAST]
    (rangeInSource fnCallAST)

assertParse :: String -> AST.SourceFile -> Assertion
assertParse source expectedAST = case Parser.parse source of
  Right ast -> assertEqual expectedAST ast
  Left err -> assertFailure $ "parse failed: " ++ show err

assertParseErrorMessage :: String -> String -> Assertion
assertParseErrorMessage source expectedMessage = case Parser.parse source of
  Right _ -> assertFailure "parse succeeded"
  Left err -> assertEqual expectedMessage (Error.message err)

arg :: Double -> (Int, Int) -> AST.Argument
arg val (loc,len) = AST.PositionalArgument $ AST.FloatLiteral val argRange
  where argRange = oneLineRange "" loc len

range :: Int -> Int -> SourceRange
range = oneLineRange ""

test_unexpected_eof =
  assertParseErrorMessage
    "foo("
    "Unexpected end of input. Expecting identifier, \"#\", float, integer, literal string or \")\"."

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

test_nested_function_call =
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

test_function_call_sans_parens =
  assertParseFnCall
    "red"
    (AST.FunctionCall
      (AST.QualifiedIdentifier ["red"] (range 1 3))
      []
      (range 1 3))

test_function_def =
  assertParse
    "let red = #ff0000"
    (AST.SourceFile
      [AST.FunctionDefStatement
        (AST.FunctionPrototype
          (AST.QualifiedIdentifier ["red"] (range 5 4))
          []
          (range 5 4))
        (AST.HexRGBLiteral "ff0000" (range 11 7))
        (range 1 17)]
      (range 1 17))

test_function_def_with_args =
  assertParse
    "let f(c) = g(c)"
    (AST.SourceFile
      [AST.FunctionDefStatement
        (AST.FunctionPrototype
          (AST.QualifiedIdentifier ["f"] (range 5 1))
          [AST.RequiredArgumentPrototype "c" (range 7 1)]
          (range 5 5))
        (AST.FunctionCallExp
          (AST.FunctionCall
            (AST.QualifiedIdentifier ["g"] (range 12 1))
            [AST.PositionalArgument
              (AST.FunctionCallExp
                (AST.FunctionCall
                  (AST.QualifiedIdentifier ["c"] (range 14 1))
                  []
                  (range 14 1)))]
              (range 12 4)))
        (range 1 15)]
      (range 1 15))
