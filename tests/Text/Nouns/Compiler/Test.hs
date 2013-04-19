{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-missing-signatures #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Text.Nouns.Compiler.Test where

import Data.Maybe (fromJust)
import Test.Framework
import Test.HUnit.Lang (Assertion)
import qualified Text.Nouns.Compiler as Compiler
import qualified Text.Nouns.Compiler.Document as D
import qualified Text.Nouns.Parser.AST as AST
import qualified Text.Nouns.Error as Error
import Text.Nouns.SourceRange (zeroRange)

{-# ANN module "HLint: ignore Use camelCase" #-}

sourceFileWithFnCall :: AST.FunctionCall -> AST.SourceFile
sourceFileWithFnCall fnCall =
  AST.SourceFile [AST.FunctionCallStatement fnCall] zeroRange

assertFnCallCompilesTo :: D.Element -> AST.FunctionCall -> Assertion
assertFnCallCompilesTo element fnCall =
  assertCompilesTo (D.Document [element]) (sourceFileWithFnCall fnCall)

assertCompilesTo :: D.Document -> AST.SourceFile -> Assertion
assertCompilesTo document source =
  assertEqual (Right document) (Compiler.compile source)

assertFnCallFails :: Compiler.CompileError -> AST.FunctionCall -> Assertion
assertFnCallFails err fnCall =
  assertEqual (Left err) $ Compiler.compile (sourceFileWithFnCall fnCall)

assertFnCallFailsWithErrorMessage :: String -> AST.FunctionCall -> Assertion
assertFnCallFailsWithErrorMessage message fnCall =
  case Compiler.compile (sourceFileWithFnCall fnCall) of
    Right _ -> assertFailure "function call did not fail"
    Left err -> assertEqual message (Error.message err)

class ToValue a where
  toValue :: a -> AST.Expression

instance ToValue Int where
  toValue x = AST.FloatLiteral (fromIntegral x) zeroRange

instance ToValue String where
  toValue s = AST.HexRGBLiteral s zeroRange

instance ToValue AST.FunctionCall where
  toValue = AST.FunctionCallExp

instance ToValue AST.Expression where
  toValue = id

arg :: (ToValue a) => a -> AST.Argument
arg x = AST.PositionalArgument $ toValue x

args :: (ToValue a) => [a] -> [AST.Argument]
args = map arg

funcName :: String -> AST.QualifiedIdentifier
funcName str = AST.QualifiedIdentifier (splitBy '.' str) zeroRange
  where splitBy _ [] = []
        splitBy x (y:ys)
          | x == y    = [] : rest
          | null ys   = [[y]]
          | otherwise = (y : head rest) : tail rest
           where rest = splitBy x ys

funcCall :: (ToValue a) => String -> [a] -> AST.FunctionCall
funcCall name posArgs = AST.FunctionCall (funcName name) (args posArgs) zeroRange

kwArg :: (ToValue a) => String -> a -> AST.Argument
kwArg key x = AST.KeywordArgument key (toValue x) zeroRange

test_compile_undefined =
  assertFnCallFails
    (Compiler.UndefinedFunctionError (funcName "shape.squircle"))
    (AST.FunctionCall (funcName "shape.squircle") [] zeroRange)

test_compile_statement_type_error = assertFnCallFails err fnCall where
  err = Compiler.StatementReturnTypeError fnCall
  fnCall = AST.FunctionCall
    (funcName "color.red")
    []
    zeroRange

test_compile_missing_args = assertFnCallFails err fnCall where
  err = Compiler.FunctionCallError fnCall (Compiler.MissingArgumentError "x")
  fnCall = AST.FunctionCall
    (funcName "shape.rectangle")
    []
    zeroRange

test_compile_literal_arg_type_error = assertFnCallFails err fnCall where
  err = Compiler.FunctionCallError fnCall (Compiler.ArgumentTypeError "fill")
  fnCall = AST.FunctionCall
    (funcName "shape.circle")
    (args [0, 0, 10, 10 :: Int])
    zeroRange

test_compile_fn_arg_type_error = assertFnCallFails err fnCall where
  err = Compiler.FunctionCallError fnCall (Compiler.ArgumentTypeError "fill")
  fnCall = AST.FunctionCall
    (funcName "shape.circle")
    (args [0, 0, 10 :: Int] ++ [fillArg])
    zeroRange
  fillArg = kwArg "fill" $ funcCall "shape.circle" [1, 2, 3 :: Int]

test_compile_too_many_args = assertFnCallFails err fnCall where
    err = Compiler.FunctionCallError fnCall Compiler.TooManyArgumentsError
    fnCall = AST.FunctionCall
      (funcName "shape.circle")
      (args [0, 0, 10 :: Int] ++ args ["ffffff", "000000"])
      zeroRange

test_compile_rectangle =
  assertFnCallCompilesTo
    (D.Rectangle 0 0 10 10 0 D.black)
    (AST.FunctionCall
      (funcName "shape.rectangle")
      (args [0, 0, 10, 10 :: Int])
      zeroRange)

test_compile_image =
  assertFnCallCompilesTo
    (D.Image 10 10 50 50 $ fromJust $ D.fileIRI "cat.jpeg")
    (AST.FunctionCall
      (funcName "image")
      (args [10, 10, 50, 50 :: Int] ++ [arg (AST.StringLiteral "cat.jpeg" zeroRange)])
      zeroRange)

test_compile_image_bad_filename = assertFnCallFails err fnCall where
    err = Compiler.FunctionCallError fnCall (Compiler.ArgumentTypeError "file")
    fnCall = AST.FunctionCall
      (funcName "image")
      (args [0, 0, 10, 10 :: Int] ++ [arg (AST.StringLiteral "http://example.com/image.png" zeroRange)])
      zeroRange

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
      zeroRange)

test_compile_rounded_rectangle =
  assertFnCallCompilesTo
    (D.Rectangle 0 0 10 10 2 D.black)
    (AST.FunctionCall
      (funcName "shape.rectangle")
      (args [0, 0, 10, 10, 2 :: Int])
      zeroRange)

test_compile_circle =
  assertFnCallCompilesTo
    (D.Circle 50 50 100 D.black)
    (AST.FunctionCall
      (funcName "shape.circle")
      (args [50, 50, 100 :: Int])
      zeroRange)

test_compile_fill_color_literal =
  assertFnCallCompilesTo
    (D.Circle 50 50 100 $ D.ColorPaint $ D.Color "123abc")
    (AST.FunctionCall
      (funcName "shape.circle")
      (args [50, 50, 100 :: Int] ++ [kwArg "fill" "123abc"])
      zeroRange)

test_compile_fill_color_fn =
  assertFnCallCompilesTo
    (D.Circle 50 50 100 $ D.ColorPaint $ D.Color "00ff00")
    (AST.FunctionCall
      (funcName "shape.circle")
      (args [50, 50, 100 :: Int] ++ [fillArg])
      zeroRange)
  where fillArg = kwArg "fill" $ funcCall "color.green" ([] :: [Int])

test_compile_fill_gradient_fn =
  assertFnCallCompilesTo
    (D.Circle 50 50 100 $ D.GradientPaint $ D.LinearGradient 90 [(0, D.Color fg), (1, D.Color bg)])
    (AST.FunctionCall
      (funcName "shape.circle")
      (args [50, 50, 100 :: Int] ++ [fillArg])
      zeroRange)
  where fillArg = arg $ funcCall "gradient.vertical" [fg, bg]
        (fg, bg) = ("#ff0000", "#00ff00")

test_compile_empty =
  assertCompilesTo
    (D.Document [])
    (AST.SourceFile
      []
      zeroRange)

test_define_function =
  assertCompilesTo
    (D.Document [])
    (AST.SourceFile
      [AST.FunctionDefStatement
        (AST.QualifiedIdentifier ["red"] zeroRange)
        (AST.HexRGBLiteral "aa0000" zeroRange)
        zeroRange]
      zeroRange)

test_call_defined_function =
  assertCompilesTo
    (D.Document
      [D.Circle 0 0 10 $ D.ColorPaint $ D.Color "000000"])
    (AST.SourceFile
      [ AST.FunctionDefStatement
          (AST.QualifiedIdentifier ["circle"] zeroRange)
          (AST.FunctionCallExp
            (AST.FunctionCall
              (AST.QualifiedIdentifier ["shape","circle"] zeroRange)
              (args [0, 0, 10 :: Int])
              zeroRange))
          zeroRange
      , AST.FunctionCallStatement
         (AST.FunctionCall
          (AST.QualifiedIdentifier ["circle"] zeroRange)
          []
          zeroRange)
      ]
      zeroRange)

test_undefined_fn_message =
  assertFnCallFailsWithErrorMessage
    "Undefined function \"foo\"."
    (AST.FunctionCall
      (funcName "foo")
      []
      zeroRange)

test_statement_return_type_message =
  assertFnCallFailsWithErrorMessage
    "Function \"color.red\" does not return an element."
    (AST.FunctionCall
      (funcName "color.red")
      []
      zeroRange)

test_missing_arg_message =
  assertFnCallFailsWithErrorMessage
    "Function \"shape.circle\" requires argument \"cx\"."
    (AST.FunctionCall
      (funcName "shape.circle")
      []
      zeroRange)

test_arg_type_message =
  assertFnCallFailsWithErrorMessage
    "Argument \"cx\" to function \"shape.circle\" has incorrect type."
    (AST.FunctionCall
      (funcName "shape.circle")
      [arg "ffffff"]
      zeroRange)

test_arg_count_message =
  assertFnCallFailsWithErrorMessage
    "Too many arguments to function \"color.red\"."
    (AST.FunctionCall
      (funcName "color.red")
      [arg "ffffff"]
      zeroRange)
