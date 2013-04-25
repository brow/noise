{-# LANGUAGE TypeSynonymInstances #-}

module Text.Nouns.Compiler.Function
( Function
, Value(..)
, FunctionError(..)
, requireArg
, acceptArg
, throw
, call
) where

import Control.Applicative
import Control.Monad
import Text.Nouns.Compiler.Error (FunctionError(..))
import qualified Text.Nouns.Compiler.Document as D

type Keyword = String

data Value = FloatValue Double
           | RGBValue String
           | StringValue String
           | ElementValue D.Element
           | GradientValue D.Gradient

data ArgStack = ArgStack [Value] [(Keyword,Value)]

data Result a = Success a ArgStack | Failure FunctionError

data Function a = Function { runFunction :: ArgStack -> Result a }

instance Functor Function where
  fmap = liftM

instance Applicative Function where
  pure = return
  (<*>) = ap

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
  fromValue :: Value -> Maybe a

instance FromValue Value where
  fromValue = Just

instance FromValue D.Number where
  fromValue (FloatValue x) = Just x
  fromValue _ = Nothing

instance FromValue D.Paint where
  fromValue (RGBValue x) = Just $ D.ColorPaint (D.Color x)
  fromValue (GradientValue x) = Just (D.GradientPaint x)
  fromValue _ = Nothing

instance FromValue D.Color where
  fromValue (RGBValue x) = Just (D.Color x)
  fromValue _ = Nothing

instance FromValue D.IRI where
  fromValue (StringValue x) = D.fileIRI x
  fromValue _ = Nothing

getArg :: (FromValue a) => Keyword -> Maybe a -> Function a
getArg keyword maybeDefault = Function $ \args -> case args of
  ArgStack (value:xs) kwargs -> case fromValue value of
    Just x     -> case lookup keyword kwargs of
      Nothing       -> Success x (ArgStack xs kwargs)
      Just _        -> Failure (RedundantKeywordArgError keyword)
    Nothing    -> Failure (ArgumentTypeError keyword)
  ArgStack [] kwargs         -> case lookup keyword kwargs of
    Just value -> case fromValue value of
      Just x        -> Success x (ArgStack [] kwargs)
      Nothing       -> Failure (ArgumentTypeError keyword)
    Nothing    -> case maybeDefault of
      Just default' -> Success default' (ArgStack [] kwargs)
      Nothing       -> Failure (MissingArgumentError keyword)

requireArg :: (FromValue a) => Keyword -> Function a
requireArg keyword = getArg keyword Nothing

acceptArg :: (FromValue a) => Keyword -> a -> Function a
acceptArg keyword default' = getArg keyword (Just default')

throw :: FunctionError -> Function a
throw err = Function $ \_ -> Failure err
