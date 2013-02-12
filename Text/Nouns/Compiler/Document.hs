module Text.Nouns.Compiler.Document
( Document(..)
, Element(..)
) where

data Document = Document [Element] deriving Show

data Element = Rectangle Integer Integer Integer Integer deriving Show
