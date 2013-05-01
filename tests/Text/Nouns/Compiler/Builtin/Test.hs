{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-missing-signatures #-}
{-# LANGUAGE QuasiQuotes #-}

module Text.Nouns.Compiler.Builtin.Test where

import Test.Framework
import Data.Maybe (fromJust)
import Data.String.QQ (s)
import Assertion
import qualified Text.Nouns.Compiler.Document as D
import qualified Text.Nouns.Compiler.Document.Color as Color

{-# ANN module "HLint: ignore Use camelCase" #-}

color :: String -> D.Color
color = fromJust . Color.fromHex

colorPaint :: String -> D.Paint
colorPaint = D.ColorPaint . color

test_shape_rectangle = assertOutputElement
  (D.Rectangle (-10) (-20) 30 40 5 $ colorPaint "abcdef")
  [s|shape.rectangle(x: -10,
                     y: -20,
                     width: 30,
                     height: 40,
                     radius: 5,
                     fill: #abcdef)|]

test_shape_circle = assertOutputElement
  (D.Circle 1 2 3 $ colorPaint "123456")
  [s|shape.circle(cx: 1,
                  cy: 2,
                  radius: 3,
                  fill: #123456)|]

test_color_red = assertOutputElement
  (D.Circle 1 2 3 $ colorPaint "ff0000")
  [s|shape.circle(1, 2, 3, color.red)|]

test_gradient_vertical = assertOutputElement
  (D.Circle 1 2 3 $
    D.GradientPaint $
      D.LinearGradient 90
        [ (0, color "123456")
        , (1, color "abcdef") ])
  [s|shape.circle(1, 2, 3, gradient.vertical(#123456, #abcdef))|]

test_gradient_horizontal = assertOutputElement
  (D.Circle 1 2 3 $
    D.GradientPaint $
      D.LinearGradient 0
        [ (0, color "a1b2c3")
        , (1, color "d4e5f6") ])
  [s|shape.circle(1, 2, 3, gradient.horizontal(#a1b2c3, #d4e5f6))|]

test_image = assertOutputElement
  (D.Image 10 10 50 50 $ fromJust $ D.fileIRI "cat.jpeg")
  [s|image(x: 10,
           y: 10,
           width: 50,
           height: 50,
           file: "cat.jpeg")|]

test_group_empty = assertOutputElement
  (D.Group [])
  [s|group|]

test_group = assertOutputElement
  (D.Group
    [ D.Circle 1 2 3 $ colorPaint "123456"
    , D.Circle 4 5 6 $ colorPaint "abcdef" ])
  [s|group with
       shape.circle(1,2,3,#123456)
       shape.circle(4,5,6,#abcdef)
     end|]

test_empty_path = assertOutputElement
  (D.Path (colorPaint "000000") [])
  [s|shape.path|]

test_path = assertOutputElement
  (D.Path
    (colorPaint "abcdef")
    [ D.Move 1 2
    , D.Line 3 4 ])
  [s|shape.path(#abcdef) with
       path.move(1,2)
       path.line(3,4)
     end|]
