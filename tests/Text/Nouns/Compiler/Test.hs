{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Text.Nouns.Compiler.Test where

import Test.Framework
import Test.HUnit.Lang (Assertion)
import qualified Text.Nouns.Compiler as Compiler
import qualified Text.Nouns.Compiler.Document as D
import qualified Text.Nouns.Parser.AST as AST

{-# ANN module "HLint: ignore Use camelCase" #-}

assertRightEqual x y = assertEqual x (Right y)
assertLeftEqual x y = assertEqual x (Left y)

assertFnCallCompilesTo :: AST.FunctionCall -> D.Element -> Assertion
assertFnCallCompilesTo fnCall element =
  assertRightEqual
    (Compiler.compile $ AST.SourceFile [fnCall])
    (D.Document [element])

assertFnCallFails :: AST.FunctionCall -> Compiler.CompileError -> Assertion
assertFnCallFails fnCall =
  assertLeftEqual $ Compiler.compile $ AST.SourceFile [fnCall]

test_compile_undefined =
  assertFnCallFails
    (AST.FunctionCall "squircle" [])
    Compiler.UndefinedFunctionError

test_compile_wrong_num_args =
  assertFnCallFails
    (AST.FunctionCall "rectangle" [])
    Compiler.WrongNumberOfArgumentsError

test_compile_rectangle =
  assertFnCallCompilesTo
    (AST.FunctionCall "rectangle" [0, 0, 10, 10])
    (D.Rectangle (D.Length 0) (D.Length 0) (D.Length 10) (D.Length 10))
