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
    (Compiler.compile $ AST.SourceFile [fnCall] AST.zeroRange)

assertFnCallFails :: Compiler.CompileError -> AST.FunctionCall -> Assertion
assertFnCallFails err fnCall =
  assertEqual (Left err) $ Compiler.compile $ AST.SourceFile [fnCall] AST.zeroRange

test_compile_undefined =
  assertFnCallFails
    (Compiler.UndefinedFunctionError AST.zeroRange)
    (AST.FunctionCall
      (AST.QualifiedIdentifier ["shape", "squircle"] AST.zeroRange)
      []
      AST.zeroRange)

test_compile_missing_args =
  assertFnCallFails
    (Compiler.FunctionCallError AST.zeroRange Compiler.MissingArgumentError)
    (AST.FunctionCall
      (AST.QualifiedIdentifier ["shape", "rectangle"] AST.zeroRange)
      []
      AST.zeroRange)

test_compile_too_many_args =
  assertFnCallFails
    (Compiler.FunctionCallError AST.zeroRange Compiler.TooManyArgumentsError)
    (AST.FunctionCall
      (AST.QualifiedIdentifier ["shape", "rectangle"] AST.zeroRange)
      [ AST.Argument 0 AST.zeroRange
      , AST.Argument 0 AST.zeroRange
      , AST.Argument 10 AST.zeroRange
      , AST.Argument 10 AST.zeroRange
      , AST.Argument 10 AST.zeroRange
      ]
      AST.zeroRange)

test_compile_rectangle =
  assertFnCallCompilesTo
    (D.Rectangle 0 0 10 10)
    (AST.FunctionCall
      (AST.QualifiedIdentifier ["shape", "rectangle"] AST.zeroRange)
      [ AST.Argument 0 AST.zeroRange
      , AST.Argument 0 AST.zeroRange
      , AST.Argument 10 AST.zeroRange
      , AST.Argument 10 AST.zeroRange
      ]
      AST.zeroRange)

test_compile_circle =
  assertFnCallCompilesTo
    (D.Circle 50 50 100)
    (AST.FunctionCall
      (AST.QualifiedIdentifier ["shape", "circle"] AST.zeroRange)
      [ AST.Argument 50 AST.zeroRange
      , AST.Argument 50 AST.zeroRange
      , AST.Argument 100 AST.zeroRange
      ]
      AST.zeroRange)
