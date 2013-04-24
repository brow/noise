{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-missing-signatures #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, QuasiQuotes #-}

module Text.Nouns.Compiler.Test where

import Test.Framework
import Data.String.QQ (s)
import Assertion
import qualified Text.Nouns.Compiler.Document as D

{-# ANN module "HLint: ignore Use camelCase" #-}

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

test_define_function_with_0_args = assertOutputElement
  (D.Circle 0 0 15 $ D.ColorPaint $ D.Color "ffff00")
  [s|let yellow = #ffff00
     shape.circle(0, 0, 15, fill:yellow)|]

test_define_function_with_many_args = assertOutputElement
  (D.Circle 0 0 20 $ D.ColorPaint $ D.Color "abcdef")
  [s|let circle(r, c) = shape.circle(0, 0,r,c)
     circle(20, #abcdef)|]
