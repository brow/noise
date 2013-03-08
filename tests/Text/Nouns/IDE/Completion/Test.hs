{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-missing-signatures #-}

module Text.Nouns.IDE.Completion.Test where

import Test.Framework
import Text.Nouns.IDE.Completion (parametersForString, Parameters(..), Dictionary(..))

{-# ANN module "HLint: ignore Use camelCase" #-}

test_empty_string = assertEqual
  (parametersForString "")
  (Parameters FunctionCallDictionary "")

test_identifier = assertEqual
  (parametersForString "fn")
  (Parameters FunctionCallDictionary "fn")

test_trailing_whitespace = assertEqual
  (parametersForString "foo()  ")
  (Parameters FunctionCallDictionary "")

test_function = assertEqual
  (parametersForString "foo() bar")
  (Parameters FunctionCallDictionary "bar")

{-test_empty_arg = assertEqual-}
  {-(parametersForString "foo() bar(")-}
  {-(Parameters-}
