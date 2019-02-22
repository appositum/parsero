module Text.Parsero.Tokenizer
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
import Data.Foldable            (foldl')
import Text.Parsero.Char
import Text.Parsero.Combinators
import Text.Parsero.Parser
import Text.Parsero.Stream      (Stream)

data IntegerOrDouble = MkInteger Integer
                     | MkDouble Double
                     deriving (Eq, Show)


token :: Stream s => Parsero s a -> Parsero s a
token p = p `surroundedBy` skipWhitespaces

symbol :: Stream s => s -> Parsero s s
symbol = token . chunk

symbolic :: Stream s => Char -> Parsero s Char
symbolic = token . char

charLiteral :: Stream s => Parsero s Char
charLiteral = notChar '\'' `surroundedBy` char '\''

stringLiteral :: Stream s => Parsero s String
stringLiteral = many (notChar '"') `surroundedBy` symbolic '"'

stringLiteral' :: Stream s => Parsero s String
stringLiteral' = many (notChar '\'') `surroundedBy` symbolic '\''

comma :: Stream s => Parsero s Char
comma = symbolic ','

dot :: Stream s => Parsero s Char
dot = symbolic '.'

colon :: Stream s => Parsero s Char
colon = symbolic ':'

semicolon :: Stream s => Parsero s Char
semicolon = symbolic ';'

parens :: Stream s => Parsero s a -> Parsero s a
parens = between (symbolic '(') (symbolic ')')

brackets :: Stream s => Parsero s a -> Parsero s a
brackets = between (symbolic '[') (symbolic ']')

braces :: Stream s => Parsero s a -> Parsero s a
braces = between (symbolic '{') (symbolic '}')

angles :: Stream s => Parsero s a -> Parsero s a
angles = between (symbolic '<') (symbolic '>')

signed :: (Num a, Stream s) => Parsero s (a -> a)
signed =  negate <$ char '-'
      <|> id <$ char '+'
      <|> pure id

natural :: Stream s => Parsero s Integer
natural = token $ read <$> some digit

integer :: Stream s => Parsero s Integer
integer = token $ signed <*> natural

double :: Stream s => Parsero s Double
double = token $ signed <*> double' where
  double' = do
    first <- some digit
    dot <- char '.'
    rest <- some digit
    pure $ read (first ++ dot : rest)

float :: Stream s => Parsero s Double
float = double

integerOrDouble :: Stream s => Parsero s IntegerOrDouble
integerOrDouble = MkDouble <$> double <|> MkInteger <$> integer

naturalOrDouble :: Stream s => Parsero s IntegerOrDouble
naturalOrDouble = MkDouble <$> double <|> MkInteger <$> natural

toDecimal :: Integer -> String -> Integer
toDecimal base =
  foldl' (\x z -> base*x + fromIntegral (digitToInt z)) 0

hex :: Stream s => Parsero s Integer
hex = token $ do
  char '0'
  oneOf "xX"
  h <- some $ digit <|> oneOf (['a'..'f'] ++ ['A'..'F'])
  pure $ toDecimal 16 h

oct :: Stream s => Parsero s Integer
oct = token $ do
  char '0'
  oneOf "oO"
  o <- some (oneOf ['0'..'7'])
  pure $ toDecimal 8 o

bin :: Stream s => Parsero s Integer
bin = token $ do
  char '0'
  oneOf "bB"
  b <- some (oneOf "01")
  pure $ toDecimal 2 b
