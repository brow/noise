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

sourceFileWithExpression :: AST.Expression -> AST.SourceFile
sourceFileWithExpression expression =
  AST.SourceFile [AST.ExpressionStatement expression] zeroRange

assertExpCompilesToElem :: D.Element -> AST.Expression -> Assertion
assertExpCompilesToElem element expression =
  assertCompilesTo (D.Document [element]) (sourceFileWithExpression expression)

assertCompilesTo :: D.Document -> AST.SourceFile -> Assertion
assertCompilesTo document source =
  assertEqual (Right document) (Compiler.compile source)

assertExpCompileError :: Compiler.CompileError -> AST.Expression -> Assertion
assertExpCompileError err expression =
  assertEqual (Left err) $ Compiler.compile (sourceFileWithExpression expression)

assertExpCompileErrorMessage :: String -> AST.Expression -> Assertion
assertExpCompileErrorMessage message expression =
  case Compiler.compile (sourceFileWithExpression expression) of
    Right _ -> assertFailure "function call did not fail"
    Left err -> assertEqual message (Error.message err)

class ToValue a where
  toValue :: a -> AST.Expression

instance ToValue Int where
  toValue x = AST.FloatLiteral (fromIntegral x) zeroRange

instance ToValue String where
  toValue s = AST.HexRGBLiteral s zeroRange

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

funcCall :: (ToValue a) => String -> [a] -> AST.Expression
funcCall name posArgs = AST.FunctionCall (funcName name) (args posArgs) zeroRange

kwArg :: (ToValue a) => String -> a -> AST.Argument
kwArg key x = AST.KeywordArgument key (toValue x) zeroRange

test_compile_undefined =
  assertExpCompileError
    (Compiler.UndefinedFunctionError (funcName "shape.squircle"))
    (AST.FunctionCall (funcName "shape.squircle") [] zeroRange)

test_compile_statement_type_error = assertExpCompileError err fnCall where
  err = Compiler.ExpressionStatementTypeError fnCall
  fnCall = AST.FunctionCall
    (funcName "color.red")
    []
    zeroRange

test_compile_missing_args = assertExpCompileError err fnCall where
  err = Compiler.FunctionCallError
    (funcName "shape.rectangle")
    (Compiler.MissingArgumentError "x")
  fnCall = AST.FunctionCall
    (funcName "shape.rectangle")
    []
    zeroRange

test_compile_literal_arg_type_error = assertExpCompileError err fnCall where
  err = Compiler.FunctionCallError
    (funcName "shape.circle")
    (Compiler.ArgumentTypeError "fill")
  fnCall = AST.FunctionCall
    (funcName "shape.circle")
    (args [0, 0, 10, 10 :: Int])
    zeroRange

test_compile_fn_arg_type_error = assertExpCompileError err fnCall where
  err = Compiler.FunctionCallError
    (funcName "shape.circle")
    (Compiler.ArgumentTypeError "fill")
  fnCall = AST.FunctionCall
    (funcName "shape.circle")
    (args [0, 0, 10 :: Int] ++ [fillArg])
    zeroRange
  fillArg = kwArg "fill" $ funcCall "shape.circle" [1, 2, 3 :: Int]

test_compile_too_many_args = assertExpCompileError err fnCall where
    err = Compiler.FunctionCallError
      (funcName "shape.circle")
      Compiler.TooManyArgumentsError
    fnCall = AST.FunctionCall
      (funcName "shape.circle")
      (args [0, 0, 10 :: Int] ++ args ["ffffff", "000000"])
      zeroRange

test_compile_rectangle =
  assertExpCompilesToElem
    (D.Rectangle 0 0 10 10 0 D.black)
    (AST.FunctionCall
      (funcName "shape.rectangle")
      (args [0, 0, 10, 10 :: Int])
      zeroRange)

test_compile_image =
  assertExpCompilesToElem
    (D.Image 10 10 50 50 $ fromJust $ D.fileIRI "cat.jpeg")
    (AST.FunctionCall
      (funcName "image")
      (args [10, 10, 50, 50 :: Int] ++ [arg (AST.StringLiteral "cat.jpeg" zeroRange)])
      zeroRange)

test_compile_image_bad_filename = assertExpCompileError err fnCall where
    err = Compiler.FunctionCallError
      (funcName "image")
      (Compiler.ArgumentTypeError "file")
    fnCall = AST.FunctionCall
      (funcName "image")
      (args [0, 0, 10, 10 :: Int] ++ [arg (AST.StringLiteral "http://example.com/image.png" zeroRange)])
      zeroRange

test_compile_keyword_args =
  assertExpCompilesToElem
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
  assertExpCompilesToElem
    (D.Rectangle 0 0 10 10 2 D.black)
    (AST.FunctionCall
      (funcName "shape.rectangle")
      (args [0, 0, 10, 10, 2 :: Int])
      zeroRange)

test_compile_circle =
  assertExpCompilesToElem
    (D.Circle 50 50 100 D.black)
    (AST.FunctionCall
      (funcName "shape.circle")
      (args [50, 50, 100 :: Int])
      zeroRange)

test_compile_fill_color_literal =
  assertExpCompilesToElem
    (D.Circle 50 50 100 $ D.ColorPaint $ D.Color "123abc")
    (AST.FunctionCall
      (funcName "shape.circle")
      (args [50, 50, 100 :: Int] ++ [kwArg "fill" "123abc"])
      zeroRange)

test_compile_fill_color_fn =
  assertExpCompilesToElem
    (D.Circle 50 50 100 $ D.ColorPaint $ D.Color "00ff00")
    (AST.FunctionCall
      (funcName "shape.circle")
      (args [50, 50, 100 :: Int] ++ [fillArg])
      zeroRange)
  where fillArg = kwArg "fill" $ funcCall "color.green" ([] :: [Int])

test_compile_fill_gradient_fn =
  assertExpCompilesToElem
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
      [AST.DefinitionStatement
        (AST.FunctionPrototype
          (AST.QualifiedIdentifier ["red"] zeroRange)
          []
          zeroRange)
        (AST.HexRGBLiteral "aa0000" zeroRange)
        zeroRange]
      zeroRange)

test_call_defined_function =
  assertCompilesTo
    (D.Document
      [D.Circle 0 0 10 $ D.ColorPaint $ D.Color "000000"])
    (AST.SourceFile
      [ AST.DefinitionStatement
          (AST.FunctionPrototype
            (AST.QualifiedIdentifier ["circle"] zeroRange)
            []
            zeroRange)
          (AST.FunctionCall
            (AST.QualifiedIdentifier ["shape","circle"] zeroRange)
            (args [0, 0, 10 :: Int])
            zeroRange)
          zeroRange
      , AST.ExpressionStatement
         (AST.FunctionCall
          (AST.QualifiedIdentifier ["circle"] zeroRange)
          []
          zeroRange)
      ]
      zeroRange)

test_undefined_fn_message =
  assertExpCompileErrorMessage
    "Undefined function \"foo\"."
    (AST.FunctionCall
      (funcName "foo")
      []
      zeroRange)

test_statement_return_type_message =
  assertExpCompileErrorMessage
    "Top-level expression is not an element."
    (AST.FunctionCall
      (funcName "color.red")
      []
      zeroRange)

test_missing_arg_message =
  assertExpCompileErrorMessage
    "Function \"shape.circle\" requires argument \"cx\"."
    (AST.FunctionCall
      (funcName "shape.circle")
      []
      zeroRange)

test_arg_type_message =
  assertExpCompileErrorMessage
    "Argument \"cx\" to function \"shape.circle\" has incorrect type."
    (AST.FunctionCall
      (funcName "shape.circle")
      [arg "ffffff"]
      zeroRange)

test_arg_count_message =
  assertExpCompileErrorMessage
    "Too many arguments to function \"color.red\"."
    (AST.FunctionCall
      (funcName "color.red")
      [arg "ffffff"]
      zeroRange)
