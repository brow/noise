{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-missing-signatures #-}

module Main where

import Test.Framework
import {-@ HTF_TESTS @-} Text.Nouns.Compiler.Test

main = htfMain htf_importedTests
