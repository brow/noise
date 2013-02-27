module Text.Nouns.IDE.Syntax
( Highlighting(..)
, highlight
) where

import Data.Monoid
import qualified Text.Nouns.Parser.AST as AST

data Highlighting = Highlighting { argumentRanges :: [AST.SourceRange] } deriving (Eq, Show)

instance Monoid Highlighting where
  mempty = empty
  mappend x y = Highlighting { argumentRanges = argumentRanges x ++ argumentRanges y }

empty :: Highlighting
empty = Highlighting []

highlight :: AST.SourceFile -> Highlighting
highlight (AST.SourceFile fnCalls) = mconcat $ map highlightFnCall fnCalls

highlightFnCall :: AST.FunctionCall -> Highlighting
highlightFnCall (AST.FunctionCall _ args _) =
  empty { argumentRanges = map range args }
  where range (AST.Argument _ r) = r
