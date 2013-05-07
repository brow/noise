{-# LANGUAGE TypeSynonymInstances #-}

module Text.Nouns.Compiler.Function
( Function
, Value(..)
, FromValue(..)
, FunctionError(..)
, requireArg
, acceptArg
, acceptBlockArgs
, throw
, call
) where

import Control.Applicative
import Control.Monad
import Text.Nouns.Compiler.Error (FunctionError(..))
import qualified Text.Nouns.Compiler.Document as D
import qualified Text.Nouns.Compiler.Document.Color as Color

type Keyword = String

data Value = NumberValue D.Number
           | ColorValue String
           | StringValue String
           | ElementValue D.Element
           | GradientValue D.Gradient
           | PathCommandValue D.PathCommand

data ArgStack = ArgStack [Value] [(Keyword,Value)] [Value]

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

call :: Function a -> [Value] -> [(Keyword,Value)] -> [Value] -> Either FunctionError a
call function posArgs kwArgs blockArgs = case result of
  Failure err                    -> Left err
  Success _ (ArgStack (_:_) _ _) -> Left TooManyArgumentsError
  Success ret _                  -> Right ret
  where result = runFunction function (ArgStack posArgs kwArgs blockArgs)

class FromValue a where
  fromValue :: Value -> Maybe a

instance FromValue Value where
  fromValue = Just

instance FromValue D.Number where
  fromValue (NumberValue x) = Just x
  fromValue _ = Nothing

instance FromValue D.Paint where
  fromValue (ColorValue x) = D.ColorPaint <$> Color.fromHex x
  fromValue (GradientValue x) = Just (D.GradientPaint x)
  fromValue _ = Nothing

instance FromValue D.Color where
  fromValue (ColorValue x) = Color.fromHex x
  fromValue _ = Nothing

instance FromValue D.IRI where
  fromValue (StringValue x) = D.fileIRI x
  fromValue _ = Nothing

instance FromValue D.Element where
  fromValue (ElementValue x) = Just x
  fromValue _ = Nothing

instance FromValue D.PathCommand where
  fromValue (PathCommandValue x) = Just x
  fromValue _ = Nothing

getArg :: (FromValue a) => Keyword -> Maybe a -> Function a
getArg keyword maybeDefault = Function $ \args -> case args of
  ArgStack (value:xs) kwArgs blockArgs -> case fromValue value of
    Just x     -> case lookup keyword kwArgs of
      Nothing       -> Success x (ArgStack xs kwArgs blockArgs)
      Just _        -> Failure (RedundantKeywordArgError keyword)
    Nothing    -> Failure (ArgumentTypeError keyword)
  ArgStack [] kwArgs blockArgs         -> case lookup keyword kwArgs of
    Just value -> case fromValue value of
      Just x        -> Success x (ArgStack [] kwArgs blockArgs)
      Nothing       -> Failure (ArgumentTypeError keyword)
    Nothing    -> case maybeDefault of
      Just default' -> Success default' (ArgStack [] kwArgs blockArgs)
      Nothing       -> Failure (MissingArgumentError keyword)

requireArg :: (FromValue a) => Keyword -> Function a
requireArg keyword = getArg keyword Nothing

acceptArg :: (FromValue a) => Keyword -> a -> Function a
acceptArg keyword default' = getArg keyword (Just default')

acceptBlockArgs :: (FromValue a) => Function [a]
acceptBlockArgs = Function $ \args@(ArgStack _ _ xs) ->
  case mapM fromValue xs of
    Just xs' -> Success xs' args
    Nothing  -> Failure BlockStatementTypeError

throw :: FunctionError -> Function a
throw err = Function $ \_ -> Failure err
