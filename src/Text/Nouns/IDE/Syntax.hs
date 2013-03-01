module Text.Nouns.IDE.Syntax
( Syntax(..)
, syntaxify
) where

import Data.Monoid
import Data.Maybe (mapMaybe)
import qualified Text.Nouns.Parser.AST as AST
import qualified Text.Nouns.Parser.Character as Character

data Syntax = Syntax { argumentRanges :: [Character.Range] } deriving (Eq, Show)

instance Monoid Syntax where
  mempty = empty
  mappend x y = Syntax { argumentRanges = argumentRanges x ++ argumentRanges y }

empty :: Syntax
empty = Syntax []

syntaxify :: String -> AST.SourceFile -> Syntax
syntaxify src (AST.SourceFile fnCalls) = mconcat $ map (syntaxifyFnCall src) fnCalls

syntaxifyFnCall :: String -> AST.FunctionCall -> Syntax
syntaxifyFnCall src (AST.FunctionCall _ args _) =
  empty { argumentRanges = mapMaybe range args }
  where range (AST.Argument _ r) = Character.rangeAt src r
