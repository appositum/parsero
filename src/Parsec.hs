module Parsec where

import Control.Applicative
import Data.Char

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
