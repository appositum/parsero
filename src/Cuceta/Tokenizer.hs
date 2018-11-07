module Cuceta.Tokenizer
  ( bin
  , double
  , float
  , hex
  , integer
  , natural
  , oct
  , symbol
  , token
  ) where

import Cuceta.Char
import Cuceta.Combinators
import Cuceta.Parser
import Data.Char
import Data.Foldable (foldl')

token :: Parser a -> Parser a
token p = do
  spaces
  v <- p
  spaces
  pure v

symbol :: String -> Parser String
symbol = token . string

signed :: Num a => Parser (a -> a)
signed =  negate <$ char '-'
      <|> id <$ char '+'
      <|> pure id

natural :: Parser Integer
natural = token $ read <$> some digit

integer :: Parser Integer
integer = token $ signed <*> natural

double :: Parser Double
double = token $ signed <*> double' where
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
hex = token $ do
  char '0'
  oneOf "xX"
  h <- some $ digit <|> oneOf (['a'..'f'] ++ ['A'..'F'])
  pure $ toDecimal 16 h

oct :: Parser Integer
oct = token $ do
  char '0'
  oneOf "oO"
  o <- some (oneOf ['0'..'7'])
  pure $ toDecimal 8 o

bin :: Parser Integer
bin = token $ do
  char '0'
  oneOf "bB"
  b <- some (oneOf "01")
  pure $ toDecimal 2 b
