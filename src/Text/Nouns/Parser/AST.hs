module Text.Nouns.Parser.AST where

data SourceFile = SourceFile [FunctionCall] deriving Show

data FunctionCall = FunctionCall FunctionName [Argument] deriving Show

type FunctionName = String

type Argument = Double
