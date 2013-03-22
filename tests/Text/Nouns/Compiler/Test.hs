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

instance ToValue AST.FunctionCall where
  toValue = AST.FunctionCallExp

instance ToValue AST.Expression where
  toValue = id

arg :: (ToValue a) => a -> AST.Argument
arg x = AST.PositionalArgument $ toValue x

args :: (ToValue a) => [a] -> [AST.Argument]
args = map arg

funcName :: String -> AST.QualifiedIdentifier
funcName str = AST.QualifiedIdentifier (splitBy '.' str) AST.zeroRange
  where splitBy _ [] = []
        splitBy x (y:ys)
          | x == y    = [] : rest
          | null ys   = [[y]]
          | otherwise = (y : head rest) : tail rest
           where rest = splitBy x ys

funcCall :: (ToValue a) => String -> [a] -> AST.FunctionCall
funcCall name posArgs = AST.FunctionCall (funcName name) (args posArgs) AST.zeroRange

kwArg :: (ToValue a) => String -> a -> AST.Argument
kwArg key x = AST.KeywordArgument key (toValue x) AST.zeroRange

test_compile_undefined =
  assertFnCallFails
    (Compiler.UndefinedFunctionError AST.zeroRange)
    (AST.FunctionCall (funcName "shape.squircle") [] AST.zeroRange)

test_compile_statement_type_error =
  assertFnCallFails
    (Compiler.StatementReturnTypeError AST.zeroRange)
    (AST.FunctionCall
      (funcName "color.red")
      []
      AST.zeroRange)

test_compile_missing_args =
  assertFnCallFails
    (Compiler.FunctionCallError AST.zeroRange (Compiler.MissingArgumentError "x"))
    (AST.FunctionCall
      (funcName "shape.rectangle")
      []
      AST.zeroRange)

test_compile_literal_arg_type_error =
  assertFnCallFails
    (Compiler.FunctionCallError AST.zeroRange (Compiler.ArgumentTypeError "fill"))
    (AST.FunctionCall
      (funcName "shape.circle")
      (args [0, 0, 10, 10 :: Int])
      AST.zeroRange)

test_compile_fn_arg_type_error =
  assertFnCallFails
    (Compiler.FunctionCallError AST.zeroRange (Compiler.ArgumentTypeError "fill"))
    (AST.FunctionCall
      (funcName "shape.circle")
      (args [0, 0, 10 :: Int] ++ [fillArg])
      AST.zeroRange)
  where fillArg = kwArg "fill" $ funcCall "shape.circle" [1, 2, 3 :: Int]

test_compile_too_many_args =
  assertFnCallFails
    (Compiler.FunctionCallError AST.zeroRange Compiler.TooManyArgumentsError)
    (AST.FunctionCall
      (funcName "shape.circle")
      (args [0, 0, 10 :: Int] ++ args ["ffffff", "000000"])
      AST.zeroRange)

test_compile_rectangle =
  assertFnCallCompilesTo
    (D.Rectangle 0 0 10 10 0 D.black)
    (AST.FunctionCall
      (funcName "shape.rectangle")
      (args [0, 0, 10, 10 :: Int])
      AST.zeroRange)

test_compile_keyword_args =
  assertFnCallCompilesTo
    (D.Rectangle 0 0 10 10 0 D.black)
    (AST.FunctionCall
      (funcName "shape.rectangle")
      [ arg (0 :: Int)
      , kwArg "width" (10 :: Int)
      , kwArg "y" (0 :: Int)
      , kwArg "height" (10 :: Int)
      ]
      AST.zeroRange)

test_compile_rounded_rectangle =
  assertFnCallCompilesTo
    (D.Rectangle 0 0 10 10 2 D.black)
    (AST.FunctionCall
      (funcName "shape.rectangle")
      (args [0, 0, 10, 10, 2 :: Int])
      AST.zeroRange)

test_compile_circle =
  assertFnCallCompilesTo
    (D.Circle 50 50 100 D.black)
    (AST.FunctionCall
      (funcName "shape.circle")
      (args [50, 50, 100 :: Int])
      AST.zeroRange)

test_compile_fill_color_literal =
  assertFnCallCompilesTo
    (D.Circle 50 50 100 $ D.ColorPaint $ D.Color "123abc")
    (AST.FunctionCall
      (funcName "shape.circle")
      (args [50, 50, 100 :: Int] ++ [kwArg "fill" "123abc"])
      AST.zeroRange)

test_compile_fill_color_fn =
  assertFnCallCompilesTo
    (D.Circle 50 50 100 $ D.ColorPaint $ D.Color "00ff00")
    (AST.FunctionCall
      (funcName "shape.circle")
      (args [50, 50, 100 :: Int] ++ [fillArg])
      AST.zeroRange)
  where fillArg = kwArg "fill" $ funcCall "color.green" ([] :: [Int])

test_compile_fill_gradient_fn =
  assertFnCallCompilesTo
    (D.Circle 50 50 100 $ D.GradientPaint $ D.LinearGradient 90 (D.Color fg) (D.Color bg))
    (AST.FunctionCall
      (funcName "shape.circle")
      (args [50, 50, 100 :: Int] ++ [fillArg])
      AST.zeroRange)
  where fillArg = arg $ funcCall "gradient.vertical" [fg, bg]
        (fg, bg) = ("#ff0000", "#00ff00")
