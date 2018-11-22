module Text.Cuceta.Parser
  ( empty
  , many
  , optional
  , some
  , Alternative
  , ParseError(..)
  , Parser(..)
  , (<|>)
  ) where

import Control.Applicative
import Text.Cuceta.Stream  (Stream)

data ParseError = EndOfStream
                | EmptyInput
                | DoesNotSatisfy
                deriving (Eq, Show)

newtype Parser s a = MkParser
  { parse :: s -> (Either ParseError a, s) }

instance Stream s => Functor (Parser s) where
  fmap f p = MkParser $ \input ->
    case parse p input of
      (Left err, rest) -> (Left err, rest)
      (Right a, rest) -> (Right (f a), rest)

instance Stream s => Applicative (Parser s) where
  pure a = MkParser $ \input -> (Right a, input)
  pf <*> px = MkParser $ \input ->
    case parse pf input of
      (Left err, rest) -> (Left err, rest)
      (Right f, rest) -> parse (f <$> px) rest

instance Stream s => Alternative (Parser s) where
  empty = MkParser $ \input -> (Left EmptyInput, input)
  p1 <|> p2 = MkParser $ \input ->
    case parse p1 input of
      (Left err, _) -> parse p2 input
      (Right a, rest) -> (Right a, rest)

instance Stream s => Monad (Parser s) where
  p >>= f = MkParser $ \input ->
    case parse p input of
      (Left err, rest) -> (Left err, rest)
      (Right a, rest) -> parse (f a) rest
