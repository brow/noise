{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-missing-signatures #-}

module Text.Noise.Parser.Test where

import Test.Framework
import Test.HUnit.Lang (Assertion)
import Assertion
import Data.Maybe
import Text.Noise.SourceRange (SourceRange, oneLineRange)
import qualified Text.Noise.Parser.AST as AST
import qualified Text.Noise.Parser as Parser
import qualified Text.Noise.Error as Error
import qualified Text.Noise.Compiler.Document as D
import qualified Text.Noise.Compiler.Document.Color as Color

assertAST :: AST.SourceFile -> String -> Assertion
assertAST ast src = case Parser.parse "" src of
  Left err -> assertFailure ("parse error: " ++ Error.message err)
  Right ast' -> assertEqual ast ast'

range :: Int -> Int -> SourceRange
range = oneLineRange ""

color :: String -> D.Color
color = fromJust . Color.fromHex

colorPaint :: String -> D.Paint
colorPaint = D.ColorPaint . color

{-# ANN module "HLint: ignore Use camelCase" #-}

test_expecting_argument = mapM
  (assertError "Unexpected end of input. Expecting argument or \")\".")
  [ "foo("
  , "foo(bar," ]

test_expecting_identifier = assertError
  "Unexpected end of input. Expecting identifier."
  "let "

test_expecting_expression = mapM
  (assertError "Unexpected end of input. Expecting expression.")
  [ "let foo ="
  , "foo +"
  , "("
  , "fn(foo:" ]

test_expecting_statement = assertError
  "Unexpected end of input. Expecting statement or \"end\"."
  "group with "

test_malformed_color_literal = mapM
  (assertError "Unexpected color format. Expecting hex color of form RRGGBB or AARRGGBB.")
  [ "#abc"
  , "#abcdef1"
  , "#abcdef12345" ]

test_trailing_comma = assertOutputElement
  D.circle
  "shape.circle(0,0,0,)"

test_argb_literal = assertOutputElement
  D.circle { D.fill = colorPaint "abcdef12" }
  "shape.circle(0,0,0,fill:#abcdef12)"

test_negative_numbers = assertOutputElement
  D.circle { D.cx = -1, D.cy = -2.34}
  "shape.circle(-1,-2.34, 0)"

test_ranges = assertAST
  (AST.SourceFile
    [AST.DefinitionStatement
      (AST.Reserved "let" (range 2 3))
      (AST.FunctionPrototype
        (AST.QualifiedIdentifier ["foo"] (range 6 3))
        [AST.RequiredArgumentPrototype "x" (range 10 1)]
        (range 6 6))
      (AST.FunctionCall
        (AST.QualifiedIdentifier ["bar"] (range 15 3))
        [AST.PositionalArgument
          (AST.FunctionCall
            (AST.QualifiedIdentifier ["baz"] (range 21 3))
            []
            Nothing
            (range 21 3))]
        Nothing
        (range 15 11))
      (range 2 24)]
    (range 2 24))
  " let foo(x) = bar ( baz ) "
