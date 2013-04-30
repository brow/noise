{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-missing-signatures #-}

module Text.Nouns.Renderer.Test where

import Test.Framework
import Debug.Trace
import Text.XML.HXT.XPath.XPathEval (getXPath)
import qualified Text.XML.HXT.Parser.XmlParsec as XmlParsec
import qualified Text.Nouns.Renderer as Renderer
import qualified Text.Nouns.Compiler.Document as D

{-# ANN module "HLint: ignore Use camelCase" #-}

test_circle = do
  let doc = D.Document [D.Circle 1 2 3 $ D.ColorPaint $ D.Color "000000"]
      xmlTree = head $ XmlParsec.xread $ Renderer.render doc
  assertBool $ not $ null $ getXPath "svg" (traceShow xmlTree $ xmlTree)
