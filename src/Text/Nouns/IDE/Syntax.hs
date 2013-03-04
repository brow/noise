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
  , functionCallRanges :: [Character.Range]
  } deriving (Eq, Show)

instance Monoid Syntax where
  mempty = empty
  mappend x y = Syntax
    { argumentRanges = argumentRanges x ++ argumentRanges y
    , identifierRanges = identifierRanges x ++ identifierRanges y
    , functionCallRanges = functionCallRanges x ++ functionCallRanges y
    }

empty :: Syntax
empty = Syntax [] [] []

syntaxify :: String -> AST.SourceFile -> Syntax
syntaxify src (AST.SourceFile fnCalls) = mconcat $ map (syntaxifyFnCall src) fnCalls

syntaxifyFnCall :: String -> AST.FunctionCall -> Syntax
syntaxifyFnCall src fnCall@(AST.FunctionCall name args _) =
  empty { identifierRanges = mapMaybe charRange [name]
        , argumentRanges = mapMaybe charRange args
        , functionCallRanges = mapMaybe charRange [fnCall]
        }
  where charRange x = Character.rangeAt src $ AST.rangeInSource x
