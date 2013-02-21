module Text.Nouns.Compiler.Function
( Function
, FunctionError(..)
, Value
, requireArg
, call
) where

type Value = Double

data ArgStack = ArgStack [Value]

data FunctionError = MissingArgumentError deriving (Show, Eq)

data Result a = Success a ArgStack | Failure FunctionError

data Function a = Function { runFunction :: ArgStack -> Result a }

instance Monad Function where
  return x = Function $ \args -> Success x args
  Function r >>= f = Function $ \args ->
    case r args of
      Failure err -> Failure err
      Success x args' ->
        let Function r' = f x
        in  r' args'

call :: Function a -> [Value] -> Either FunctionError a
call function values = case result of
  Failure err -> Left err
  Success ret _ -> Right ret
  where
    result = runFunction function (ArgStack values)

requireArg :: Function Value
requireArg = Function $ \args -> case args of
  ArgStack (x:xs) -> Success x (ArgStack xs)
  ArgStack [] -> Failure MissingArgumentError
