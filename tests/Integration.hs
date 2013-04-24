{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-missing-signatures #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, QuasiQuotes #-}

module Integration where

import Test.Framework
import Test.HUnit.Lang (Assertion)
import Data.Maybe (fromJust)
import Data.String.QQ (s)
import qualified Text.Nouns.Error as Error
import qualified Text.Nouns.Parser as Parser
import qualified Text.Nouns.Compiler as Compiler
import qualified Text.Nouns.Compiler.Document as D

{-# ANN module "HLint: ignore Use camelCase" #-}

data CompileResult = Output D.Document
                   | CompileError Compiler.CompileError
                   | ParseError Parser.ParseError

compile :: String -> CompileResult
compile src = case Parser.parse src of
  Left err  -> ParseError err
  Right ast -> case Compiler.compile ast of
    Left err  -> CompileError err
    Right doc -> Output doc

assertError :: String -> String -> Assertion
assertError msg src = case compile src of
  ParseError err   -> checkMsg err
  CompileError err -> checkMsg err
  _                -> assertFailure "no error"
  where checkMsg err = assertEqual msg (Error.message err)

assertOutput :: D.Document -> String -> Assertion
assertOutput expected src = case compile src of
  Output doc       -> assertEqual expected doc
  ParseError err   -> failWithErr err
  CompileError err -> failWithErr err
  where failWithErr err = assertFailure ("error: " ++ Error.message err)

assertOutputElement :: D.Element -> String -> Assertion
assertOutputElement = assertOutput . D.Document . (:[])

test_empty = assertOutput (D.Document []) ""

test_undefined_function = assertError
  "Undefined function \"shape.squircle\"."
  "shape.squircle"

test_top_level_expression_type_error = assertError
  "Top-level expression is not an element."
  "color.red"

test_argument_type_error = assertError
  "Argument \"cx\" to function \"shape.circle\" has incorrect type."
  "shape.circle(cx:#ffffff)"

test_bad_filename = assertError
  "Argument \"file\" to function \"image\" has incorrect type."
  "image(0,0,50,50,\"http://example.com/image.png\")"

test_missing_argument = assertError
  "Function \"shape.circle\" requires argument \"cx\"."
  "shape.circle"

text_excess_argument = assertError
  "Too many arguments to function \"color.red\"."
  "color.red(#ff0000)"

test_shape_rectangle = assertOutputElement
  (D.Rectangle (-10) (-20) 30 40 5 $ D.ColorPaint $ D.Color "abcdef")
  [s|shape.rectangle(x: -10,
                     y: -20,
                     width: 30,
                     height: 40,
                     radius: 5,
                     fill: #abcdef)|]

test_shape_circle = assertOutputElement
  (D.Circle 1 2 3 $
    D.ColorPaint $
      D.Color "123456")
  [s|shape.circle(cx: 1,
                  cy: 2,
                  radius: 3,
                  fill: #123456)|]

test_color_red = assertOutputElement
  (D.Circle 1 2 3 $
    D.ColorPaint $
      D.Color "ff0000")
  [s|shape.circle(1, 2, 3, color.red)|]

test_gradient_vertical = assertOutputElement
  (D.Circle 1 2 3 $
    D.GradientPaint $
      D.LinearGradient 90
        [ (0, D.Color "123456")
        , (1, D.Color "abcdef") ])
  [s|shape.circle(1, 2, 3, gradient.vertical(#123456, #abcdef))|]

test_gradient_horizontal = assertOutputElement
  (D.Circle 1 2 3 $
    D.GradientPaint $
      D.LinearGradient 0
        [ (0, D.Color "a1b2c3")
        , (1, D.Color "d4e5f6") ])
  [s|shape.circle(1, 2, 3, gradient.horizontal(#a1b2c3, #d4e5f6))|]

test_image = assertOutputElement
  (D.Image 10 10 50 50 $ fromJust $ D.fileIRI "cat.jpeg")
  [s|image(x: 10,
           y: 10,
           width: 50,
           height: 50,
           file: "cat.jpeg")|]
