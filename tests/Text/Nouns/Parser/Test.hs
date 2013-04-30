{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-missing-signatures #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Text.Nouns.Parser.Test where

import Test.Framework
import Test.HUnit.Lang (Assertion)
import Assertion
import Text.Nouns.SourceRange (SourceRange, oneLineRange)
import Text.Nouns.Parser.AST as AST
import Text.Nouns.Parser as Parser
import Text.Nouns.Error as Error

assertAST :: AST.SourceFile -> String -> Assertion
assertAST ast src = case Parser.parse src of
  Left err -> assertFailure ("parse error: " ++ Error.message err)
  Right ast' -> assertEqual ast ast'

range :: Int -> Int -> SourceRange
range = oneLineRange ""

{-# ANN module "HLint: ignore Use camelCase" #-}

test_expecting_argument = assertError
  "Unexpected end of input. Expecting argument or \")\"."
  "foo("

test_expecting_identifier = assertError
  "Unexpected end of input. Expecting identifier."
  "let "

test_expecting_expression = mapM
  (assertError "Unexpected end of input. Expecting expression.")
  [ "let foo ="
  , "fn(foo:" ]

test_ranges = assertAST
  (AST.SourceFile
    [AST.DefinitionStatement
      (AST.Reserved "let" (range 2 3))
      (AST.FunctionPrototype
        (AST.QualifiedIdentifier ["foo"] (range 6 3))
        [AST.RequiredArgumentPrototype "x" (range 10 1)]
        (range 6 6))
      (AST.FunctionCall
        (AST.QualifiedIdentifier ["bar"] (range 15 3))
        [AST.PositionalArgument
          (AST.FunctionCall
            (AST.QualifiedIdentifier ["baz"] (range 21 3))
            []
            Nothing
            (range 21 3))]
        Nothing
        (range 15 11))
      (range 2 24)]
    (range 2 24))
  " let foo(x) = bar ( baz ) "
