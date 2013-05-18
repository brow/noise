{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-missing-signatures #-}
{-# LANGUAGE QuasiQuotes #-}

module Text.Noise.Compiler.Test where

import Test.Framework
import Data.String.QQ (s)
import Data.Maybe
import Assertion
import Text.Noise.SourceRange (oneLineRange)
import qualified Text.Noise.Compiler.Document as D
import qualified Text.Noise.Compiler.Document.Color as Color

{-# ANN module "HLint: ignore Use camelCase" #-}

colorPaint :: String -> D.Paint
colorPaint = D.ColorPaint . fromJust. Color.fromHex

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

test_block_statement_type_error = assertError
  "Statement in block of function \"group\" has incorrect type."
  "group with #123456 end"

test_bad_filename = assertError
  "Argument \"file\" to function \"image\" has incorrect type."
  "image(0,0,50,50,\"http://example.com/image.png\")"

test_missing_argument = assertError
  "Function \"shape.circle\" requires argument \"cx\"."
  "shape.circle"

text_excess_argument = assertError
  "Too many arguments to function \"color.red\"."
  "color.red(#ff0000)"

test_positional_arg_after_keyword_arg = assertErrorAt (oneLineRange "" 34 7)
  "Positional argument follows a keyword argument."
  "gradient.horizontal(from:#abcdef,#123456)"

test_keyword_arg_duplicating_positional_arg = assertError
  "Keyword argument \"from\" duplicates a positional argument."
  "gradient.horizontal(#abcdef, from: #123456, to: #000000)"

test_keyword_arg_duplicating_keyword_arg = assertError
  "Duplicate keyword argument \"from\" in function call."
  "gradient.horizontal(from: #abcdef, to: #123456, from: #000000)"

test_duplicate_args_in_function_def = assertError
  "Duplicate argument \"x\" in function definition."
  "let fn(x,y,x) = color.red"

test_define_function_with_0_args = assertOutputElement
  D.circle { D.fill = colorPaint "ffff00"}
  [s|let color.yellow = #ffff00
     shape.circle(0, 0, 0, fill:color.yellow)|]

test_define_function_with_many_args = assertOutputElement
  D.circle { D.r = 20, D.fill = colorPaint "abcdef" }
  [s|let circle(r, c) = shape.circle(0, 0,r,c)
     circle(20, #abcdef)|]

test_argument_shadows_function = assertOutputElement
  D.circle { D.fill = colorPaint "123456" }
  [s|let x = #abcdef
     let f(x) = x
     shape.circle(0, 0, 0, f(#123456))|]

test_operators = assertOutputElement
  D.rectangle { D.x = 1+2, D.y = 3-4, D.width = 5*6, D.height = 7/8 }
  [s|shape.rectangle(1+2, 3-4, 5*6, 7/8)|]

test_operators_associativity = assertOutputElement
  D.rectangle { D.x = 1-2-3, D.y = 4/5/6, D.width = 1-2+3, D.height = 4/5*6 }
  [s|shape.rectangle(1-2-3, 4/5/6, 1-2+3, 4/5*6)|]

test_operators_precedence = assertOutputElement
  D.rectangle { D.x = 2+3*4, D.y = 2+3/4, D.width = 2-3*4, D.height = 2-3/4, D.cornerRadius = (2+3)*4 }
  [s|shape.rectangle(2+3*4, 2+3/4, 2-3*4, 2-3/4, (2+3)*4)|]
