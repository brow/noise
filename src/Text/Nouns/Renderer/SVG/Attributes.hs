module Text.Nouns.Renderer.SVG.Attributes where

import Text.Blaze.Internal (Attribute, stringValue)
import qualified Text.Blaze.Svg11.Attributes as SVG
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

offset :: D.Number -> Attribute
offset = SVG.offset . stringValue . show

stopColor :: D.Color -> Attribute
stopColor = SVG.stopColor . stringValue . show

id :: String -> Attribute
id = SVG.id_ . stringValue

x1 :: D.Number -> Attribute
x1 = SVG.x1 . stringValue . show

y1 :: D.Number -> Attribute
y1 = SVG.y1 . stringValue . show

x2 :: D.Number -> Attribute
x2 = SVG.x2 . stringValue . show

y2 :: D.Number -> Attribute
y2 = SVG.y2 . stringValue . show

xlinkHref :: D.IRI -> Attribute
xlinkHref = SVG.xlinkHref . stringValue . show

d :: String -> Attribute
d = SVG.d . stringValue

preserveaspectratio :: String -> Attribute
preserveaspectratio = SVG.preserveaspectratio . stringValue

fillOpacity :: D.OpacityValue -> Attribute
fillOpacity = SVG.fillOpacity . stringValue . show
