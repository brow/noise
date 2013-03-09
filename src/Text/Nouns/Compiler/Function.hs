module Text.Nouns.Compiler.Function
( Function
, FunctionError(..)
, Value
, requireArg
, acceptArg
, call
) where

type Keyword = String

type Value = Double

data ArgStack = ArgStack [Value] [(Keyword,Value)]

data FunctionError = MissingArgumentError Keyword
                   | TooManyArgumentsError
                   deriving (Show, Eq)

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

call :: Function a -> [Value] -> [(Keyword,Value)] -> Either FunctionError a
call function args kwargs = case result of
  Failure err                  -> Left err
  Success _ (ArgStack (_:_) _) -> Left TooManyArgumentsError
  Success ret _                -> Right ret
  where result = runFunction function (ArgStack args kwargs)

class FromValue a where
  fromValue :: Value -> a

instance FromValue Double where
  fromValue = id

getArg :: (FromValue a) => Keyword -> Maybe a -> Function a
getArg keyword maybeDefault = Function $ \args -> case args of
  ArgStack (x:xs) kwargs -> Success (fromValue x) (ArgStack xs kwargs)
  ArgStack [] kwargs -> case lookup keyword kwargs of
    Just value -> Success (fromValue value) (ArgStack [] kwargs)
    Nothing -> case maybeDefault of
      Just default' -> Success default' (ArgStack [] kwargs)
      Nothing -> Failure (MissingArgumentError keyword)

requireArg :: (FromValue a) => Keyword -> Function a
requireArg keyword = getArg keyword Nothing

acceptArg :: (FromValue a) => Keyword -> a -> Function a
acceptArg keyword default' = getArg keyword (Just default')
