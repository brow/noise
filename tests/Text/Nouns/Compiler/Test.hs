{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-missing-signatures #-}

module Text.Nouns.Compiler.Test where

import Test.Framework
import Test.HUnit.Lang (Assertion)
import qualified Text.Nouns.Compiler as Compiler
import qualified Text.Nouns.Compiler.Document as D
import qualified Text.Nouns.Parser.AST as AST

{-# ANN module "HLint: ignore Use camelCase" #-}

assertFnCallCompilesTo :: D.Element -> AST.FunctionCall -> Assertion
assertFnCallCompilesTo element fnCall =
  assertEqual
    (Right $ D.Document [element])
    (Compiler.compile $ AST.SourceFile [fnCall])

assertFnCallFails :: Compiler.CompileError -> AST.FunctionCall -> Assertion
assertFnCallFails err fnCall =
  assertEqual (Left err) $ Compiler.compile $ AST.SourceFile [fnCall]

test_compile_undefined =
  assertFnCallFails
    Compiler.UndefinedFunctionError
    (AST.FunctionCall "squircle" [])

test_compile_wrong_num_args =
  assertFnCallFails
    (Compiler.FunctionCallError Compiler.MissingArgumentError)
    (AST.FunctionCall "rectangle" [])

test_compile_rectangle =
  assertFnCallCompilesTo
    (D.Rectangle 0 0 10 10)
    (AST.FunctionCall "rectangle" [0, 0, 10, 10])

test_compile_circle =
  assertFnCallCompilesTo
    (D.Circle 50 50 100)
    (AST.FunctionCall "circle" [50, 50, 100])
