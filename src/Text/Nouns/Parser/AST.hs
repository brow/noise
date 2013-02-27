module Text.Nouns.Parser.AST
( SourceRange
, sourceRange
, SourceFile(..)
, FunctionCall(..)
, QualifiedIdentifier(..)
, Identifier
, Argument
) where

import qualified Text.Parsec.Pos as Parsec

type SourceRange = (Parsec.SourcePos, Parsec.SourcePos)

data SourceFile = SourceFile [FunctionCall] deriving (Show, Eq)

data FunctionCall = FunctionCall QualifiedIdentifier [Argument] SourceRange deriving (Show, Eq)

data QualifiedIdentifier = QualifiedIdentifier [Identifier] deriving (Show, Eq)

type Identifier = String

type Argument = Double

sourceRange :: Parsec.SourceName -> Int -> SourceRange
sourceRange name len = ( Parsec.newPos name 1 1
                       , Parsec.newPos name 1 (1 + len))
