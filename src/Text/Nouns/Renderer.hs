{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Text.Nouns.Renderer (render) where

import           Numeric (showHex)
import           Data.Monoid
import           Data.List (foldl')
import           Data.ByteString (unpack)
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Text.Blaze.Internal as Blaze
import qualified Text.Blaze.Svg11 as SVG
import qualified Text.Blaze.Svg11.Attributes as SVG.At
import           Text.Blaze.Svg11 ((!), Svg)
import qualified Text.Blaze.Svg.Renderer.Pretty as Pretty
import qualified Text.Blaze.Svg.Renderer.Utf8 as Utf8
import qualified Text.Nouns.Renderer.SVG.Attributes as At
import qualified Text.Nouns.Compiler.Document as D

class Renderable a where
  renderToInlineSvg :: a -> InlineSvg
  renderToSvg :: a -> Svg
  render :: a -> String
  renderToInlineSvg = InlineSvg [] . renderToSvg
  renderToSvg = uninline . renderToInlineSvg
  render = Pretty.renderSvg . renderToSvg

data InlineSvg = InlineSvg [Svg] Svg

data InlineAttribute = InlineAttribute (Maybe Svg) SVG.Attribute

instance Monoid InlineSvg where
  mempty = InlineSvg [] mempty
  mappend (InlineSvg defs svg) (InlineSvg defs' svg') =
    InlineSvg (defs <> defs') (svg <> svg')

(?) :: InlineSvg -> InlineAttribute -> InlineSvg
(InlineSvg defs svg) ? (InlineAttribute attrDef attr) = InlineSvg defs' svg'
  where svg' = svg ! attr
        defs' = case attrDef of
          Just def -> def : defs
          Nothing -> defs

inline :: Svg -> InlineSvg
inline = InlineSvg []

uninline :: InlineSvg -> Svg
uninline (InlineSvg defs main) = SVG.defs (mconcat defs) <> main

instance Blaze.Attributable InlineSvg where
  (InlineSvg defs main) ! attr = InlineSvg defs (main ! attr)

instance Renderable D.Document where
  renderToSvg (D.Document elems) =
    SVG.docTypeSvg $ uninline $ mconcat $ map renderToInlineSvg elems

instance Renderable D.Element where
  renderToInlineSvg (D.Rectangle x y w h radius fill) = inline SVG.rect
    ! At.x x
    ! At.y y
    ! At.width w
    ! At.height h
    ! At.rx radius
    ? fillAttr fill

  renderToInlineSvg (D.Circle cx cy r fill) = inline SVG.circle
    ! At.cx cx
    ! At.cy cy
    ! At.r r
    ? fillAttr fill

instance Renderable D.Gradient where
  renderToSvg (D.LinearGradient angle color0 color1) = SVG.lineargradient
    ! At.x2 (cos radians)
    ! At.y2 (sin radians)
    $ do
      SVG.stop
        ! At.offset 0
        ! At.stopColor color0
      SVG.stop
        ! At.offset 1
        ! At.stopColor color1
    where radians = angle * pi / 180

strAttr :: (SVG.AttributeValue -> SVG.Attribute) -> String -> InlineAttribute
strAttr attrFn = InlineAttribute Nothing . attrFn . Blaze.stringValue

svgAttr :: (Renderable a) => (SVG.AttributeValue -> SVG.Attribute) -> a -> InlineAttribute
svgAttr attrFn x = InlineAttribute (Just svg') $ attrFn (Blaze.stringValue funcIRI)
  where svg = renderToSvg x
        svg' = svg ! At.id uniqueId
        funcIRI = D.showFuncIRI (D.localIRIForId uniqueId)
        uniqueId = foldl' (flip showHex) "" $ unpack sha
        sha = SHA1.hashlazy (Utf8.renderSvg svg)

fillAttr :: D.Paint -> InlineAttribute
fillAttr (D.ColorPaint color) = strAttr SVG.At.fill (show color)
fillAttr (D.GradientPaint gradient) = svgAttr SVG.At.fill gradient
