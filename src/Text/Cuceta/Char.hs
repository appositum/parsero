module Text.Cuceta.Char
  ( alpha
  , alphaNum
  , anyChar
  , char
  , digit
  , letter
  , lower
  , newline
  , notChar
  , space
  , whitespaces
  , string
  , tab
  , upper
  ) where

import Text.Cuceta.Combinators
import Text.Cuceta.Parser
import Data.Char

char :: Char -> Parser Char
char c = satisfy (==c)

space :: Parser Char
space = satisfy isSpace

whitespaces :: Parser ()
whitespaces = skipMany space

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

anyChar :: Parser Char
anyChar = satisfy (const True)

notChar :: Char -> Parser Char
notChar c = satisfy (/=c)

string :: String -> Parser String
string "" = pure ""
string ccs@(c:cs) = char c *> string cs *> pure ccs

digit :: Parser Char
digit = satisfy isDigit

tab :: Parser Char
tab = char '\t'

newline :: Parser Char
newline = char '\n'
