module Text.Nouns.Renderer (render) where

import Text.Nouns.Compiler.Document
import Text.Blaze.Svg11
import Text.Blaze.Svg.Renderer.Pretty (renderSvg)
import Data.Monoid (mempty)

render :: Document -> String
render = renderSvg . renderToSvg 

renderToSvg :: Document -> Svg
renderToSvg _ = docTypeSvg mempty
