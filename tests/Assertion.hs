{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-unused-binds #-}

module Assertion
( assertError
, assertErrorAt
, assertOutput
, assertOutputElement
) where

import Test.Framework
import Test.HUnit.Lang (Assertion)
import qualified Text.Noise.Error as Error
import qualified Text.Noise.Parser as Parser
import qualified Text.Noise.Compiler as Compiler
import qualified Text.Noise.Compiler.Document as D
import qualified Text.Noise.SourceRange as SourceRange

data CompileResult = Output D.Document
                   | CompileError Compiler.CompileError
                   | ParseError Parser.ParseError

compile :: String -> CompileResult
compile src = case Parser.parse "" src of
  Left err  -> ParseError err
  Right ast -> case Compiler.compile ast of
    Left err  -> CompileError err
    Right doc -> Output doc

assertError :: String -> String -> Assertion
assertError = assertErrorRangeAndMessage Nothing

assertErrorAt :: SourceRange.SourceRange -> String -> String -> Assertion
assertErrorAt range = assertErrorRangeAndMessage (Just range)

assertErrorRangeAndMessage :: Maybe SourceRange.SourceRange -> String -> String -> Assertion
assertErrorRangeAndMessage range msg src = case compile src of
  ParseError err   -> check err
  CompileError err -> check err
  _                -> assertFailure "no error"
  where check err = do
          assertEqual msg (Error.message err)
          case range of
            Just r -> assertEqual r (SourceRange.rangeInSource err)
            Nothing -> return ()

assertOutput :: D.Document -> String -> Assertion
assertOutput expected src = case compile src of
  Output doc       -> assertEqual expected doc
  ParseError err   -> failWithErr err
  CompileError err -> failWithErr err
  where failWithErr err = assertFailure ("error: " ++ Error.message err)

assertOutputElement :: D.Element -> String -> Assertion
assertOutputElement = assertOutput . D.Document . (:[])
