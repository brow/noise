module Text.Nouns.Parser.AST
( SourceRange
, oneLineRange
, zeroRange
, SourceFile(..)
, FunctionCall(..)
, QualifiedIdentifier(..)
, Identifier
, Argument(..)
) where

import qualified Text.Parsec.Pos as Parsec

type SourceRange = (Parsec.SourcePos, Parsec.SourcePos)

data SourceFile = SourceFile [FunctionCall] deriving (Show, Eq)

data FunctionCall = FunctionCall QualifiedIdentifier [Argument] SourceRange deriving (Show, Eq)

data QualifiedIdentifier = QualifiedIdentifier [Identifier] SourceRange deriving (Show, Eq)

type Identifier = String

data Argument = Argument Double SourceRange deriving (Show, Eq)

oneLineRange :: Parsec.SourceName -> Int -> Int -> SourceRange
oneLineRange name col len = ( Parsec.newPos name 1 col
                            , Parsec.newPos name 1 (col + len))

zeroRange :: SourceRange
zeroRange = oneLineRange "" 1 0
