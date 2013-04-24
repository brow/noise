{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-missing-signatures #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, QuasiQuotes #-}

module Text.Nouns.Compiler.Builtin.Test where

import Test.Framework
import Data.Maybe (fromJust)
import Data.String.QQ (s)
import Assertion
import qualified Text.Nouns.Compiler.Document as D

{-# ANN module "HLint: ignore Use camelCase" #-}

test_shape_rectangle = assertOutputElement
  (D.Rectangle (-10) (-20) 30 40 5 $ D.ColorPaint $ D.Color "abcdef")
  [s|shape.rectangle(x: -10,
                     y: -20,
                     width: 30,
                     height: 40,
                     radius: 5,
                     fill: #abcdef)|]

test_shape_circle = assertOutputElement
  (D.Circle 1 2 3 $
    D.ColorPaint $
      D.Color "123456")
  [s|shape.circle(cx: 1,
                  cy: 2,
                  radius: 3,
                  fill: #123456)|]

test_color_red = assertOutputElement
  (D.Circle 1 2 3 $
    D.ColorPaint $
      D.Color "ff0000")
  [s|shape.circle(1, 2, 3, color.red)|]

test_gradient_vertical = assertOutputElement
  (D.Circle 1 2 3 $
    D.GradientPaint $
      D.LinearGradient 90
        [ (0, D.Color "123456")
        , (1, D.Color "abcdef") ])
  [s|shape.circle(1, 2, 3, gradient.vertical(#123456, #abcdef))|]

test_gradient_horizontal = assertOutputElement
  (D.Circle 1 2 3 $
    D.GradientPaint $
      D.LinearGradient 0
        [ (0, D.Color "a1b2c3")
        , (1, D.Color "d4e5f6") ])
  [s|shape.circle(1, 2, 3, gradient.horizontal(#a1b2c3, #d4e5f6))|]

test_image = assertOutputElement
  (D.Image 10 10 50 50 $ fromJust $ D.fileIRI "cat.jpeg")
  [s|image(x: 10,
           y: 10,
           width: 50,
           height: 50,
           file: "cat.jpeg")|]
