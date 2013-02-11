module Text.Nouns.Compiler.Document where

data Document = Document [Element] deriving Show

data Element = Rectangle { x :: Integer
                         , y :: Integer
                         , width :: Integer
                         , height :: Integer
                         } deriving Show
