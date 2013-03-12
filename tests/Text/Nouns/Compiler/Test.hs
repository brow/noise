{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-missing-signatures #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Text.Nouns.Compiler.Test where

import Test.Framework
import Test.HUnit.Lang (Assertion)
import qualified Text.Nouns.Compiler as Compiler
import qualified Text.Nouns.Compiler.Document as D
import qualified Text.Nouns.Parser.AST as AST

{-# ANN module "HLint: ignore Use camelCase" #-}

sourceFileWithFnCall :: AST.FunctionCall -> AST.SourceFile
sourceFileWithFnCall fnCall =
  AST.SourceFile [AST.FunctionCallStatement fnCall] AST.zeroRange

assertFnCallCompilesTo :: D.Element -> AST.FunctionCall -> Assertion
assertFnCallCompilesTo element fnCall =
  assertEqual
    (Right $ D.Document [element])
    (Compiler.compile $ sourceFileWithFnCall fnCall)

assertFnCallFails :: Compiler.CompileError -> AST.FunctionCall -> Assertion
assertFnCallFails err fnCall =
  assertEqual (Left err) $ Compiler.compile (sourceFileWithFnCall fnCall)

class ToValue a where
  toValue :: a -> AST.Expression

instance ToValue Int where
  toValue x = AST.FloatLiteral (fromIntegral x) AST.zeroRange

instance ToValue String where
  toValue s = AST.HexRGBLiteral s AST.zeroRange

arg :: (ToValue a) => a -> AST.Argument
arg x = AST.PositionalArgument $ toValue x

args :: (ToValue a) => [a] -> [AST.Argument]
args = map arg

kwArg :: String -> Double -> AST.Argument
kwArg key x = AST.KeywordArgument key
  (AST.FloatLiteral x AST.zeroRange)
  AST.zeroRange

test_compile_undefined =
  assertFnCallFails
    (Compiler.UndefinedFunctionError AST.zeroRange)
    (AST.FunctionCall
      (AST.QualifiedIdentifier ["shape", "squircle"] AST.zeroRange)
      []
      AST.zeroRange)

test_compile_statement_type_error =
  assertFnCallFails
    (Compiler.StatementReturnTypeError AST.zeroRange)
    (AST.FunctionCall
      (AST.QualifiedIdentifier ["color", "red"] AST.zeroRange)
      []
      AST.zeroRange)

test_compile_missing_args =
  assertFnCallFails
    (Compiler.FunctionCallError AST.zeroRange (Compiler.MissingArgumentError "x"))
    (AST.FunctionCall
      (AST.QualifiedIdentifier ["shape", "rectangle"] AST.zeroRange)
      []
      AST.zeroRange)

test_compile_type_error =
  assertFnCallFails
    (Compiler.FunctionCallError AST.zeroRange (Compiler.ArgumentTypeError "fill"))
    (AST.FunctionCall
      (AST.QualifiedIdentifier ["shape", "circle"] AST.zeroRange)
      (args [0, 0, 10, 10 :: Int])
      AST.zeroRange)

test_compile_too_many_args =
  assertFnCallFails
    (Compiler.FunctionCallError AST.zeroRange Compiler.TooManyArgumentsError)
    (AST.FunctionCall
      (AST.QualifiedIdentifier ["shape", "circle"] AST.zeroRange)
      (args [0, 0, 10 :: Int] ++ args ["ffffff", "000000"])
      AST.zeroRange)

test_compile_rectangle =
  assertFnCallCompilesTo
    (D.Rectangle 0 0 10 10 0 D.black)
    (AST.FunctionCall
      (AST.QualifiedIdentifier ["shape", "rectangle"] AST.zeroRange)
      (args [0, 0, 10, 10 :: Int])
      AST.zeroRange)

test_compile_keyword_args =
  assertFnCallCompilesTo
    (D.Rectangle 0 0 10 10 0 D.black)
    (AST.FunctionCall
      (AST.QualifiedIdentifier ["shape", "rectangle"] AST.zeroRange)
      [arg (0 :: Int), kwArg "width" 10, kwArg "y" 0, kwArg "height" 10]
      AST.zeroRange)

test_compile_rounded_rectangle =
  assertFnCallCompilesTo
    (D.Rectangle 0 0 10 10 2 D.black)
    (AST.FunctionCall
      (AST.QualifiedIdentifier ["shape", "rectangle"] AST.zeroRange)
      (args [0, 0, 10, 10, 2 :: Int])
      AST.zeroRange)

test_compile_circle =
  assertFnCallCompilesTo
    (D.Circle 50 50 100 D.black)
    (AST.FunctionCall
      (AST.QualifiedIdentifier ["shape", "circle"] AST.zeroRange)
      (args [50, 50, 100 :: Int])
      AST.zeroRange)
