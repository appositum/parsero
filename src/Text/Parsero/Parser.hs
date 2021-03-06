module Text.Parsero.Parser
  ( empty
  , many
  , optional
  , some
  , Alternative
  , Parsero(..)
  , ParseError(..)
  , (<|>)
  ) where

import Control.Applicative
import Text.Parsero.Stream (Stream)

data ParseError = EndOfStream
                | EmptyInput
                | DoesNotSatisfy
                deriving (Eq, Show)

newtype Parsero s a = MkParsero
  { parse :: s -> (Either ParseError a, s) }

instance Stream s => Functor (Parsero s) where
  fmap f p = MkParsero $ \input ->
    case parse p input of
      (Left err, rest) -> (Left err, rest)
      (Right a, rest) -> (Right (f a), rest)

instance Stream s => Applicative (Parsero s) where
  pure a = MkParsero $ \input -> (Right a, input)
  pf <*> px = MkParsero $ \input ->
    case parse pf input of
      (Left err, rest) -> (Left err, rest)
      (Right f, rest) -> parse (f <$> px) rest

instance Stream s => Alternative (Parsero s) where
  empty = MkParsero $ \input -> (Left EmptyInput, input)
  p1 <|> p2 = MkParsero $ \input ->
    case parse p1 input of
      (Left err, _) -> parse p2 input
      (Right a, rest) -> (Right a, rest)

instance Stream s => Monad (Parsero s) where
  p >>= f = MkParsero $ \input ->
    case parse p input of
      (Left err, rest) -> (Left err, rest)
      (Right a, rest) -> parse (f a) rest
