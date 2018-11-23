module Text.Cuceta.Tokenizer
  ( angles
  , bin
  , braces
  , brackets
  , charLiteral
  , colon
  , comma
  , double
  , dot
  , float
  , hex
  , integer
  , integerOrDouble
  , natural
  , naturalOrDouble
  , oct
  , parens
  , semicolon
  , stringLiteral
  , stringLiteral'
  , symbol
  , symbolic
  , token
  ) where

import Data.Char
import Data.Foldable           (foldl')
import Text.Cuceta.Char
import Text.Cuceta.Combinators
import Text.Cuceta.Parser
import Text.Cuceta.Stream      (Stream)

data IntegerOrDouble = MkInteger Integer
                     | MkDouble Double
                     deriving (Eq, Show)


token :: Stream s => Parser s a -> Parser s a
token p = p `surroundedBy` skipWhitespaces

symbol :: Stream s => s -> Parser s s
symbol = token . chunk

symbolic :: Stream s => Char -> Parser s Char
symbolic = token . char

charLiteral :: Stream s => Parser s Char
charLiteral = notChar '\'' `surroundedBy` char '\''

stringLiteral :: Stream s => Parser s String
stringLiteral = many (notChar '"') `surroundedBy` symbolic '"'

stringLiteral' :: Stream s => Parser s String
stringLiteral' = many (notChar '\'') `surroundedBy` symbolic '\''

comma :: Stream s => Parser s Char
comma = symbolic ','

dot :: Stream s => Parser s Char
dot = symbolic '.'

colon :: Stream s => Parser s Char
colon = symbolic ':'

semicolon :: Stream s => Parser s Char
semicolon = symbolic ';'

parens :: Stream s => Parser s a -> Parser s a
parens = between (symbolic '(') (symbolic ')')

brackets :: Stream s => Parser s a -> Parser s a
brackets = between (symbolic '[') (symbolic ']')

braces :: Stream s => Parser s a -> Parser s a
braces = between (symbolic '{') (symbolic '}')

angles :: Stream s => Parser s a -> Parser s a
angles = between (symbolic '<') (symbolic '>')

signed :: (Num a, Stream s) => Parser s (a -> a)
signed =  negate <$ char '-'
      <|> id <$ char '+'
      <|> pure id

natural :: Stream s => Parser s Integer
natural = token $ read <$> some digit

integer :: Stream s => Parser s Integer
integer = token $ signed <*> natural

double :: Stream s => Parser s Double
double = token $ signed <*> double' where
  double' = do
    first <- some digit
    dot <- char '.'
    rest <- some digit
    pure $ read (first ++ dot : rest)

float :: Stream s => Parser s Double
float = double

integerOrDouble :: Stream s => Parser s IntegerOrDouble
integerOrDouble = MkDouble <$> double <|> MkInteger <$> integer

naturalOrDouble :: Stream s => Parser s IntegerOrDouble
naturalOrDouble = MkDouble <$> double <|> MkInteger <$> natural

toDecimal :: Integer -> String -> Integer
toDecimal base =
  foldl' (\x z -> base*x + fromIntegral (digitToInt z)) 0

hex :: Stream s => Parser s Integer
hex = token $ do
  char '0'
  oneOf "xX"
  h <- some $ digit <|> oneOf (['a'..'f'] ++ ['A'..'F'])
  pure $ toDecimal 16 h

oct :: Stream s => Parser s Integer
oct = token $ do
  char '0'
  oneOf "oO"
  o <- some (oneOf ['0'..'7'])
  pure $ toDecimal 8 o

bin :: Stream s => Parser s Integer
bin = token $ do
  char '0'
  oneOf "bB"
  b <- some (oneOf "01")
  pure $ toDecimal 2 b
