module Cuceta.Parser
  ( many
  , optional
  , some
  , ParseError(..)
  , Parser(..)
  , (<|>)
  ) where

import Control.Applicative

data ParseError = EndOfStream
                | EmptyInput
                | DoesNotSatisfy
                deriving (Eq, Show)

newtype Parser a = MkParser
  { parse :: String -> (Either ParseError a, String) }

instance Functor Parser where
  fmap f p = MkParser $ \input ->
    case parse p input of
      (Left err, rest) -> (Left err, rest)
      (Right a, rest) -> (Right (f a), rest)

instance Applicative Parser where
  pure a = MkParser $ \input -> (Right a, input)
  pf <*> px = MkParser $ \input ->
    case parse pf input of
      (Left err, rest) -> (Left err, rest)
      (Right f, rest) -> parse (f <$> px) rest

instance Alternative Parser where
  empty = MkParser $ \input -> (Left EmptyInput, input)
  p1 <|> p2 = MkParser $ \input ->
    case parse p1 input of
      (Left err, _) -> parse p2 input
      (Right a, rest) -> (Right a, rest)

instance Monad Parser where
  p >>= f = MkParser $ \input ->
    case parse p input of
      (Left err, rest) -> (Left err, rest)
      (Right a, rest) -> parse (f a) rest
