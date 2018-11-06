module Cuceta.Char
  ( alpha
  , alphaNum
  , bin
  , char
  , digit
  , double
  , float
  , hex
  , integer
  , letter
  , lower
  , natural
  , oct
  , space
  , spaces
  , string
  , upper
  ) where

import Cuceta.Combinators
import Cuceta.Parser
import Data.Char
import Data.Foldable

char :: Char -> Parser Char
char c = satisfy (==c)

space :: Parser Char
space = satisfy isSpace

spaces :: Parser ()
spaces :: skipMany space

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
    pure $ read (first ++ dot : rest)

float :: Parser Double
float = double

toDecimal :: Integer -> String -> Integer
toDecimal base =
  foldl' (\x z -> base*x + fromIntegral (digitToInt z)) 0

hex :: Parser Integer
hex = char '0' *> oneOf "xX" *>
  some hexDigit >>= pure . toDecimal 16 where
    hexDigit = digit <|> oneOf (['a'..'f'] ++ ['A'..'F'])

oct :: Parser Integer
oct = char '0' *> oneOf "oO" *>
  some (oneOf ['0'..'7']) >>= pure . toDecimal 8

bin :: Parser Integer
bin = char '0' *> oneOf "bB" *>
  some (oneOf "01") >>= pure . toDecimal 2
