module Text.Nouns.Parser.AST
( SourceFile(..)
, QualifiedIdentifier(..)
, Identifier
, IdentifierPath
, OperatorFunction(..)
, Operator(..)
, Argument(..)
, Expression(..)
, Statement(..)
, Reserved(..)
, FunctionPrototype(..)
, ArgumentPrototype(..)
, Block(..)
) where

import Text.Nouns.SourceRange (SourceRange, HasSourceRange(..))

data SourceFile = SourceFile [Statement] SourceRange deriving (Show, Eq)

data QualifiedIdentifier = QualifiedIdentifier IdentifierPath SourceRange deriving (Show, Eq)

type Identifier = String

data OperatorFunction = Add | Sub | Mul | Div deriving (Show, Eq)

type IdentifierPath = [Identifier]

data Reserved = Reserved String SourceRange deriving (Show, Eq)

data Statement = ExpressionStatement Expression
               | DefinitionStatement Reserved FunctionPrototype Expression SourceRange
               deriving (Show, Eq)

data FunctionPrototype = FunctionPrototype QualifiedIdentifier [ArgumentPrototype] SourceRange deriving (Show, Eq)

data ArgumentPrototype = RequiredArgumentPrototype Identifier SourceRange deriving (Show, Eq)

data Operator = Operator OperatorFunction SourceRange deriving (Show, Eq)

data Expression = FloatLiteral Double SourceRange
                | ColorLiteral String SourceRange
                | StringLiteral String SourceRange
                | FunctionCall QualifiedIdentifier [Argument] (Maybe Block) SourceRange
                | InfixOperation Operator Expression Expression
                | PrefixOperation Operator Expression
                deriving (Show, Eq)

data Argument = KeywordArgument Identifier Expression SourceRange
              | PositionalArgument Expression
              deriving (Show, Eq)

data Block = Block Reserved [Statement] Reserved SourceRange deriving (Show, Eq)

instance HasSourceRange SourceFile where
  rangeInSource (SourceFile _ r) = r

instance HasSourceRange QualifiedIdentifier where
  rangeInSource (QualifiedIdentifier _ r) = r

instance HasSourceRange Operator where
  rangeInSource (Operator _ r) = r

instance HasSourceRange Expression where
  rangeInSource (FloatLiteral _ r) = r
  rangeInSource (ColorLiteral _ r) = r
  rangeInSource (StringLiteral _ r) = r
  rangeInSource (FunctionCall _ _ _ r) = r
  rangeInSource (InfixOperation _ e0 e1) = (fst (rangeInSource e0), snd (rangeInSource e1))
  rangeInSource (PrefixOperation op expr) = (fst (rangeInSource op), snd (rangeInSource expr))

instance HasSourceRange Argument where
  rangeInSource (KeywordArgument _ _ r) = r
  rangeInSource (PositionalArgument expr) = rangeInSource expr

instance HasSourceRange Statement where
  rangeInSource (ExpressionStatement expression) = rangeInSource expression
  rangeInSource (DefinitionStatement _ _ _ r) = r

instance HasSourceRange FunctionPrototype where
  rangeInSource (FunctionPrototype _ _ r) = r

instance HasSourceRange ArgumentPrototype where
  rangeInSource (RequiredArgumentPrototype _ r) = r

instance HasSourceRange Reserved where
  rangeInSource (Reserved _ r) = r

instance HasSourceRange Block where
  rangeInSource (Block _ _ _ r) = r
