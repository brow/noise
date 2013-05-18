{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-missing-signatures #-}

module Main where

import Test.Framework
import {-@ HTF_TESTS @-} Text.Noise.Compiler.Test
import {-@ HTF_TESTS @-} Text.Noise.Compiler.Builtin.Test
import {-@ HTF_TESTS @-} Text.Noise.Compiler.Document.Color.Test
import {-@ HTF_TESTS @-} Text.Noise.Parser.Test
import {-@ HTF_TESTS @-} Text.Noise.Parser.Character.Test

main = htfMain htf_importedTests
