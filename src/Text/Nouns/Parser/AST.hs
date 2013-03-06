module Text.Nouns.Parser.AST
( SourceRange
, HasSourceRange
, rangeInSource
, oneLineRange
, zeroRange
, SourceFile(..)
, FunctionCall(..)
, QualifiedIdentifier(..)
, Identifier
, Argument(..)
, Value(..)
) where

import qualified Text.Parsec.Pos as Parsec

type SourceRange = (Parsec.SourcePos, Parsec.SourcePos)

data SourceFile = SourceFile [FunctionCall] SourceRange deriving (Show, Eq)

data FunctionCall = FunctionCall QualifiedIdentifier [Argument] SourceRange deriving (Show, Eq)

data QualifiedIdentifier = QualifiedIdentifier [Identifier] SourceRange deriving (Show, Eq)

type Identifier = String

data Value = Value Double SourceRange deriving (Show, Eq)

data Argument = KeywordArgument Identifier Value SourceRange
              | PositionalArgument Value SourceRange
              deriving (Show, Eq)

oneLineRange :: Parsec.SourceName -> Int -> Int -> SourceRange
oneLineRange name col len = ( Parsec.newPos name 1 col
                            , Parsec.newPos name 1 (col + len))

zeroRange :: SourceRange
zeroRange = oneLineRange "" 1 0

class HasSourceRange a where
  rangeInSource :: a -> SourceRange

instance HasSourceRange SourceFile where
  rangeInSource (SourceFile _ r) = r

instance HasSourceRange QualifiedIdentifier where
  rangeInSource (QualifiedIdentifier _ r) = r

instance HasSourceRange Argument where
  rangeInSource (KeywordArgument _ _ r) = r
  rangeInSource (PositionalArgument _ r) = r

instance HasSourceRange FunctionCall where
  rangeInSource (FunctionCall _ _ r) = r
