module Text.Nouns.Renderer (render) where

import qualified Text.Blaze.Svg11 as SVG
import           Text.Blaze.Svg11 ((!), Svg)
import           Text.Blaze.Svg.Renderer.Pretty (renderSvg)
import qualified Text.Nouns.Renderer.SVG.Attributes as At
import qualified Text.Nouns.Compiler.Document as D

class Renderable a where
  renderToSvg :: a -> Svg
  render :: a -> String
  render = renderSvg . renderToSvg

instance Renderable D.Document where
  renderToSvg (D.Document elems) = SVG.docTypeSvg (mapM_ renderToSvg elems)

instance Renderable D.Element where
  renderToSvg (D.Rectangle x y w h) =
    SVG.rect ! At.x x ! At.y y ! At.width w ! At.height h
