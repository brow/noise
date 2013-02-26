module Text.Nouns.Parser.AST where

data SourceFile = SourceFile [FunctionCall] deriving (Show, Eq)

data FunctionCall = FunctionCall QualifiedIdentifier [Argument] deriving (Show, Eq)

data QualifiedIdentifier = QualifiedIdentifier [Identifier] deriving (Show, Eq)

type Identifier = String

type Argument = Double
