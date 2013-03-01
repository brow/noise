module Text.Nouns.IDE.Syntax
( Highlighting(..)
, highlight
) where

import Data.Monoid
import Data.Maybe (mapMaybe)
import qualified Text.Nouns.Parser.AST as AST
import qualified Text.Nouns.Parser.Character as Character

data Highlighting = Highlighting { argumentRanges :: [Character.Range] } deriving (Eq, Show)

instance Monoid Highlighting where
  mempty = empty
  mappend x y = Highlighting { argumentRanges = argumentRanges x ++ argumentRanges y }

empty :: Highlighting
empty = Highlighting []

highlight :: String -> AST.SourceFile -> Highlighting
highlight src (AST.SourceFile fnCalls) = mconcat $ map (highlightFnCall src) fnCalls

highlightFnCall :: String -> AST.FunctionCall -> Highlighting
highlightFnCall src (AST.FunctionCall _ args _) =
  empty { argumentRanges = mapMaybe range args }
  where range (AST.Argument _ r) = Character.rangeAt src r
