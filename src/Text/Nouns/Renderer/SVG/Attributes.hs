module Text.Nouns.Renderer.SVG.Attributes where

import Text.Blaze.Svg11.Attributes as SVG
import Text.Blaze.Internal (Attribute, stringValue)
import qualified Text.Nouns.Compiler.Document as D

x :: D.Coordinate -> Attribute
x = SVG.x . stringValue . show

y :: D.Coordinate -> Attribute
y = SVG.y . stringValue . show

width :: D.Length -> Attribute
width = SVG.width . stringValue . show

height :: D.Length -> Attribute
height = SVG.height . stringValue . show

cx :: D.Coordinate -> Attribute
cx = SVG.cx . stringValue . show

cy :: D.Coordinate -> Attribute
cy = SVG.cy . stringValue . show

r :: D.Length -> Attribute
r = SVG.r . stringValue . show

rx :: D.Coordinate -> Attribute
rx = SVG.rx . stringValue . show

fill :: D.Color -> Attribute
fill = SVG.fill . stringValue . ('#' :) . D.hexString

