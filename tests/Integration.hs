{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-missing-signatures #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Integration where

import Test.Framework
import Test.HUnit.Lang (Assertion)
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

test_empty = assertOutput (D.Document []) ""

test_undefined_function = assertError
  "Undefined function \"foo\"."
  "foo"

test_top_level_expression_type_error = assertError
  "Top-level expression is not an element."
  "color.red"

test_argument_type_error = assertError
  "Argument \"cx\" to function \"shape.circle\" has incorrect type."
  "shape.circle(cx:#ffffff)"

test_missing_argument = assertError
  "Function \"shape.circle\" requires argument \"cx\"."
  "shape.circle"

text_excess_argument = assertError
  "Too many arguments to function \"color.red\"."
  "color.red(#ff0000)"
