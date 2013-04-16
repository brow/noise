module Text.Nouns.Parser.AST
( SourceFile(..)
, FunctionCall(..)
, QualifiedIdentifier(..)
, Identifier
, Argument(..)
, Expression(..)
, Statement(..)
) where

import Text.Nouns.SourceRange (SourceRange, HasSourceRange(..))

data SourceFile = SourceFile [Statement] SourceRange deriving (Show, Eq)

data FunctionCall = FunctionCall QualifiedIdentifier [Argument] SourceRange deriving (Show, Eq)

data QualifiedIdentifier = QualifiedIdentifier [Identifier] SourceRange deriving (Show, Eq)

type Identifier = String

data Statement = FunctionCallStatement FunctionCall deriving (Show, Eq)

data Expression = FloatLiteral Double SourceRange
                | HexRGBLiteral String SourceRange
                | StringLiteral String SourceRange
                | FunctionCallExp FunctionCall
                deriving (Show, Eq)

data Argument = KeywordArgument Identifier Expression SourceRange
              | PositionalArgument Expression
              deriving (Show, Eq)

instance HasSourceRange SourceFile where
  rangeInSource (SourceFile _ r) = r

instance HasSourceRange QualifiedIdentifier where
  rangeInSource (QualifiedIdentifier _ r) = r

instance HasSourceRange Expression where
  rangeInSource (FloatLiteral _ r) = r
  rangeInSource (HexRGBLiteral _ r) = r
  rangeInSource (StringLiteral _ r) = r
  rangeInSource (FunctionCallExp fnCall) = rangeInSource fnCall

instance HasSourceRange Argument where
  rangeInSource (KeywordArgument _ _ r) = r
  rangeInSource (PositionalArgument expr) = rangeInSource expr

instance HasSourceRange FunctionCall where
  rangeInSource (FunctionCall _ _ r) = r

instance HasSourceRange Statement where
  rangeInSource (FunctionCallStatement fnCall) = rangeInSource fnCall
