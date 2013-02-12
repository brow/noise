module Text.Nouns.Compiler (compile) where

import qualified Text.Nouns.Parser.AST as AST
import Text.Nouns.Compiler.Document

compile :: AST.SourceFile -> Document
compile (AST.SourceFile funcCalls) = Document (map runBuiltin funcCalls)

runBuiltin :: AST.FunctionCall -> Element
runBuiltin (AST.FunctionCall "rectangle" [x, y, w, h]) = Rectangle x y w h
runBuiltin (AST.FunctionCall name _) = error ("No builtin function '" ++ name ++ "'")
