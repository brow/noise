module Text.Nouns.Renderer (render) where

import qualified Text.Blaze.Svg11 as SVG
import           Text.Blaze.Svg11 ((!), Svg)
import           Text.Blaze.Svg.Renderer.Pretty (renderSvg)
import qualified Text.Nouns.Renderer.SVG.Attributes as At
import Text.Nouns.Compiler.Document

class Renderable a where
  renderToSvg :: a -> Svg
  render :: a -> String
  render = renderSvg . renderToSvg

instance Renderable Document where
  renderToSvg (Document elems) = SVG.docTypeSvg (mapM_ renderToSvg elems)

instance Renderable Element where
  renderToSvg (Rectangle x y w h) =
    SVG.rect ! At.x x ! At.y y ! At.width w ! At.height h
