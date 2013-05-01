{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-missing-signatures #-}

module Main where

import Test.Framework
import {-@ HTF_TESTS @-} Text.Nouns.Compiler.Test
import {-@ HTF_TESTS @-} Text.Nouns.Compiler.Builtin.Test
import {-@ HTF_TESTS @-} Text.Nouns.Compiler.Document.Color.Test
import {-@ HTF_TESTS @-} Text.Nouns.Parser.Test
import {-@ HTF_TESTS @-} Text.Nouns.Parser.Character.Test

main = htfMain htf_importedTests
