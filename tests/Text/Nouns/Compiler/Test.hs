{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-missing-signatures #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Text.Nouns.Compiler.Test where

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

test_compile_fill_gradient_fn =
  assertExpCompilesToElem
    (D.Circle 50 50 100 $ D.GradientPaint $ D.LinearGradient 90 [(0, D.Color fg), (1, D.Color bg)])
    (AST.FunctionCall
      (funcName "shape.circle")
      (args [50, 50, 100 :: Int] ++ [fillArg])
      zeroRange)
  where fillArg = arg $ funcCall "gradient.vertical" [fg, bg]
        (fg, bg) = ("#ff0000", "#00ff00")

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

test_call_defined_function_with_arg =
  assertCompilesTo
    (D.Document
      [D.Circle 0 0 15 $ D.ColorPaint $ D.Color "000000"])
    (AST.SourceFile
      [ AST.DefinitionStatement
          (AST.FunctionPrototype
            (AST.QualifiedIdentifier ["circle"] zeroRange)
            [AST.RequiredArgumentPrototype "r" zeroRange]
            zeroRange)
          (AST.FunctionCall
            (AST.QualifiedIdentifier ["shape","circle"] zeroRange)
            [ arg (0 :: Int)
            , arg (0 :: Int)
            , AST.PositionalArgument
              (AST.FunctionCall
                (AST.QualifiedIdentifier ["r"] zeroRange)
                []
                zeroRange)
            ]
            zeroRange)
          zeroRange
      , AST.ExpressionStatement
          (AST.FunctionCall
            (AST.QualifiedIdentifier ["circle"] zeroRange)
            [arg (15 :: Int)]
            zeroRange)
      ]
      zeroRange)
