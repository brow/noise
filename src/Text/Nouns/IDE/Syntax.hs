module Text.Nouns.IDE.Syntax
( Syntax(..)
, syntaxify
) where

import Data.Monoid
import Data.Maybe (mapMaybe)
import qualified Text.Nouns.Parser.AST as AST
import qualified Text.Nouns.Parser.Character as Character

data Syntax = Syntax
  { argumentRanges :: [Character.Range]
  , identifierRanges :: [Character.Range]
  } deriving (Eq, Show)

instance Monoid Syntax where
  mempty = empty
  mappend x y = Syntax
    { argumentRanges = argumentRanges x ++ argumentRanges y
    , identifierRanges = identifierRanges x ++ identifierRanges y
    }

empty :: Syntax
empty = Syntax [] []

syntaxify :: String -> AST.SourceFile -> Syntax
syntaxify src (AST.SourceFile fnCalls) = mconcat $ map (syntaxifyFnCall src) fnCalls

syntaxifyFnCall :: String -> AST.FunctionCall -> Syntax
syntaxifyFnCall src (AST.FunctionCall name args _) =
  empty { identifierRanges = mapMaybe idRange [name]
        , argumentRanges = mapMaybe argRange args
        }
  where argRange (AST.Argument _ r) = Character.rangeAt src r
        idRange (AST.QualifiedIdentifier _ r) = Character.rangeAt src r
