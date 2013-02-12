module Text.Nouns.Renderer.SVG.Attributes where

import Text.Blaze.Svg11.Attributes as SVG
import Text.Blaze.Internal (Attribute, stringValue)
import Text.Nouns.Compiler.Document (Coordinate, Length)

x :: Coordinate -> Attribute
x = SVG.x . stringValue . show

y :: Coordinate -> Attribute
y = SVG.y . stringValue . show

width :: Length -> Attribute
width = SVG.width . stringValue . show

height :: Length -> Attribute
height = SVG.height . stringValue . show
