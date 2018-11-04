module Parsec where

import Control.Applicative
import Data.Char

newtype Parser a = MkParser
  { parse :: String -> [(a, String)] }

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

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = MkParser $ \input ->
  case input of
    [] -> []
    (x:xs) -> if p x then [(x, xs)] else []

oneOf :: [Char] -> Parser Char
oneOf = satisfy . flip elem

noneOf :: [Char] -> Parser Char
noneOf = satisfy . flip notElem

char :: Char -> Parser Char
char c = satisfy (==c)

space :: Parser Char
space = satisfy isSpace

lower :: Parser Char
lower = satisfy isLower

upper :: Parser Char
upper = satisfy isUpper

alphaNum :: Parser Char
alphaNum = satisfy isAlphaNum

alpha :: Parser Char
alpha = satisfy isAlpha

letter :: Parser Char
letter = alpha

string :: String -> Parser String
string "" = pure ""
string ccs@(c:cs) = char c *> string cs *> pure ccs

digit :: Parser Char
digit = satisfy isDigit

signed :: Num a => Parser (a -> a)
signed =  negate <$ char '-'
      <|> id <$ char '+'
      <|> pure id

natural :: Parser Integer
natural = read <$> some digit

integer :: Parser Integer
integer = signed <*> natural

double :: Parser Double
double = signed <*> double' where
  double' = do
    first <- some digit
    dot <- char '.'
    rest <- some digit
    pure $ read $ first ++ (dot : rest)
