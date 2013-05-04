{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Text.Nouns.Renderer (render) where

import           Data.List
import           Data.Monoid
import           Data.Function
import           Data.Maybe
import           Control.Monad
import           Control.Applicative
import qualified Numeric
import qualified Data.List as List
import qualified Data.ByteString as ByteString
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Text.Blaze.Internal as Blaze
import qualified Text.Blaze.Svg11 as SVG
import qualified Text.Blaze.Svg11.Attributes as SVG.At
import           Text.Blaze.Svg11 ((!), Svg)
import qualified Text.Blaze.Svg.Renderer.Pretty as Pretty
import qualified Text.Blaze.Svg.Renderer.Utf8 as Utf8
import qualified Text.Nouns.Renderer.SVG.Attributes as At
import qualified Text.Nouns.Compiler.Document as D
import qualified Text.Nouns.Compiler.Document.Color as Color

class Renderable a where
  renderToInlineSvg :: a -> InlineSvg
  renderToSvg :: a -> Svg
  render :: a -> String
  renderToInlineSvg = InlineSvg [] . renderToSvg
  renderToSvg = uninline . renderToInlineSvg
  render = Pretty.renderSvg . renderToSvg

instance (Renderable a) => Renderable [a] where
  renderToInlineSvg = mconcat . map renderToInlineSvg

data InlineSvg = InlineSvg [(String, Svg)] Svg

data InlineAttribute = InlineAttribute (Maybe (String, Svg)) SVG.Attribute

instance Monoid InlineSvg where
  mempty = InlineSvg [] mempty
  mappend (InlineSvg defs svg) (InlineSvg defs' svg') =
    InlineSvg (List.unionBy ((==) `on` fst) defs defs') (svg <> svg')

(?) :: InlineSvg -> InlineAttribute -> InlineSvg
(InlineSvg defs svg) ? (InlineAttribute attrDef attr) = InlineSvg defs' svg'
  where svg' = svg ! attr
        defs' = case attrDef of
          Just def -> def : defs
          Nothing -> defs

(??) :: InlineSvg -> [InlineAttribute] -> InlineSvg
inlineSvg ?? attrs = foldl' (?) inlineSvg attrs

inline :: Svg -> InlineSvg
inline = InlineSvg []

uninline :: InlineSvg -> Svg
uninline (InlineSvg defs main) = SVG.defs (mconcat $ map snd defs) <> main

instance Blaze.Attributable InlineSvg where
  (!) (InlineSvg defs main) attr = InlineSvg defs (main ! attr)

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
    ?? fillAttrs fill

  renderToInlineSvg (D.Circle cx cy r fill) = inline SVG.circle
    ! At.cx cx
    ! At.cy cy
    ! At.r r
    ?? fillAttrs fill

  renderToInlineSvg (D.Image x y w h file) = inline SVG.image
    ! At.x x
    ! At.y y
    ! At.width w
    ! At.height h
    ! At.xlinkHref file
    ! At.preserveaspectratio "none"

  renderToInlineSvg (D.Path fill commands) = inline SVG.path
    ! At.d (concatMap renderPathCommand commands)
    ?? fillAttrs fill

  renderToInlineSvg (D.Group members) = InlineSvg defs (SVG.g innerSvg)
    where InlineSvg defs innerSvg = renderToInlineSvg members

instance Renderable D.Gradient where
  renderToSvg gradient = svgGradient $ forM_ (D.stops gradient) $ \(offset, color) ->
    SVG.stop
      ! At.offset offset
      ! At.stopColor color
    where
      svgGradient = case gradient of
        (D.RadialGradient _ ) -> SVG.radialgradient
        (D.LinearGradient angle _ ) ->
          let radians = angle * pi / 180
          in SVG.lineargradient
            ! At.x2 (cos radians)
            ! At.y2 (sin radians)

strAttr :: (SVG.AttributeValue -> SVG.Attribute) -> String -> InlineAttribute
strAttr attrFn = InlineAttribute Nothing . attrFn . Blaze.stringValue

colorAttr :: (SVG.AttributeValue -> SVG.Attribute) -> D.Color -> InlineAttribute
colorAttr attrFn = strAttr attrFn . ('#' :) . Color.toRGBHex

opacityAttr :: (SVG.AttributeValue -> SVG.Attribute) -> D.OpacityValue -> InlineAttribute
opacityAttr attrFn = InlineAttribute Nothing . attrFn . Blaze.stringValue . show

svgAttr :: (Renderable a) => (SVG.AttributeValue -> SVG.Attribute) -> a -> InlineAttribute
svgAttr attrFn x = InlineAttribute (Just (uniqueId, svg')) $ attrFn (Blaze.stringValue funcIRI)
  where svg = renderToSvg x
        svg' = svg ! At.id uniqueId
        funcIRI = D.showFuncIRI (D.localIRIForId uniqueId)
        uniqueId = List.foldl' (flip Numeric.showHex) "" $ ByteString.unpack sha
        sha = SHA1.hashlazy (Utf8.renderSvg svg)

fillAttrs :: D.Paint -> [InlineAttribute]
fillAttrs (D.GradientPaint gradient) = [ svgAttr SVG.At.fill gradient ]
fillAttrs (D.ColorPaint color) = fillAttr : maybeToList fillOpacityAttr
  where fillOpacityAttr = opacityAttr SVG.At.fillOpacity <$> Color.alpha color
        fillAttr = colorAttr SVG.At.fill color

renderPathCommand :: D.PathCommand -> String
renderPathCommand command = unwords $ case command of
  D.Move x y -> ["m", show x, show y]
  D.Line x y -> ["l", show x, show y]
