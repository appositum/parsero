module Cuceta.Char
  ( alpha
  , alphaNum
  , char
  , digit
  , letter
  , lower
  , space
  , spaces
  , string
  , upper
  ) where

import Cuceta.Combinators
import Cuceta.Parser
import Data.Char

char :: Char -> Parser Char
char c = satisfy (==c)

space :: Parser Char
space = satisfy isSpace

spaces :: Parser ()
spaces = skipMany space

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
