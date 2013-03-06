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

posArg :: Double -> AST.Argument
posArg x = AST.PositionalArgument
  (AST.Value x AST.zeroRange)
  AST.zeroRange

posArgs :: [Double] -> [AST.Argument]
posArgs = map posArg

kwArg :: String -> Double -> AST.Argument
kwArg key x = AST.KeywordArgument key
        (AST.Value x AST.zeroRange)
        AST.zeroRange

test_compile_undefined =
  assertFnCallFails
    (Compiler.UndefinedFunctionError AST.zeroRange)
    (AST.FunctionCall
      (AST.QualifiedIdentifier ["shape", "squircle"] AST.zeroRange)
      []
      AST.zeroRange)

test_compile_missing_args =
  assertFnCallFails
    (Compiler.FunctionCallError AST.zeroRange (Compiler.MissingArgumentError "x"))
    (AST.FunctionCall
      (AST.QualifiedIdentifier ["shape", "rectangle"] AST.zeroRange)
      []
      AST.zeroRange)

test_compile_too_many_args =
  assertFnCallFails
    (Compiler.FunctionCallError AST.zeroRange Compiler.TooManyArgumentsError)
    (AST.FunctionCall
      (AST.QualifiedIdentifier ["shape", "circle"] AST.zeroRange)
      (posArgs [0, 0, 10, 10])
      AST.zeroRange)

test_compile_rectangle =
  assertFnCallCompilesTo
    (D.Rectangle 0 0 10 10 0)
    (AST.FunctionCall
      (AST.QualifiedIdentifier ["shape", "rectangle"] AST.zeroRange)
      (posArgs [0, 0, 10, 10])
      AST.zeroRange)


test_compile_keyword_args =
  assertFnCallCompilesTo
    (D.Rectangle 0 0 10 10 0)
    (AST.FunctionCall
      (AST.QualifiedIdentifier ["shape", "rectangle"] AST.zeroRange)
      [posArg 0, kwArg "width" 10, kwArg "y" 0, kwArg "height" 10]
      AST.zeroRange)

test_compile_rounded_rectangle =
  assertFnCallCompilesTo
    (D.Rectangle 0 0 10 10 2)
    (AST.FunctionCall
      (AST.QualifiedIdentifier ["shape", "rectangle"] AST.zeroRange)
      (posArgs [0, 0, 10, 10, 2])
      AST.zeroRange)

test_compile_circle =
  assertFnCallCompilesTo
    (D.Circle 50 50 100)
    (AST.FunctionCall
      (AST.QualifiedIdentifier ["shape", "circle"] AST.zeroRange)
      (posArgs [50, 50, 100])
      AST.zeroRange)
