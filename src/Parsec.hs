module Parsec where

import Control.Applicative

newtype Parser a = MkParser { parse :: String -> [(a, String)] }

instance Functor Parser where
  fmap f p = MkParser $ \input ->
    case parse p input of
      [] -> []
      [(x, xs)] -> [(f x, xs)]

instance Applicative Parser where
  pure a = MkParser $ \input -> [(a, input)]
  pf <*> px = MkParser $ \input ->
    case parse pf input of
      [] -> []
      [(f, rest)] -> parse (f <$> px) rest

instance Alternative Parser where
  empty = MkParser $ const []
  p1 <|> p2 = MkParser $ \input ->
    case parse p1 input of
      [] -> parse p2 input
      [(x, xs)] -> [(x, xs)]

instance Monad Parser where
  p >>= f = MkParser $ \input ->
    case parse p input of
      [] -> []
      [(x, xs)] -> parse (f x) xs

item :: Parser Char
item = MkParser $ \input ->
  case input of
    [] -> []
    (x:xs) -> [(x, xs)]
