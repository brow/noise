{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import Test.Framework

{-# ANN module "HLint: ignore Use camelCase" #-}

test_basic = assertEqual [1,2,3] [1,2,3]

prop_basic :: [Int] -> Bool
prop_basic xs = xs == reverse (reverse xs)

main :: IO ()
main = htfMain htf_thisModulesTests
