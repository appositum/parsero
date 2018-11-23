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

import Text.Cuceta.Char
import Text.Cuceta.Combinators
import Text.Cuceta.Parser
import Data.Char
import Data.Foldable (foldl')

data IntegerOrDouble = MkInteger Integer
                     | MkDouble Double
                     deriving (Eq, Show)

token :: Parser a -> Parser a
token p = p `surroundedBy` skipWhitespaces

symbol :: String -> Parser String
symbol = token . string

symbolic :: Char -> Parser Char
symbolic = token . char

charLiteral :: Parser Char
charLiteral = notChar '\'' `surroundedBy` char '\''

stringLiteral :: Parser String
stringLiteral = many (notChar '"') `surroundedBy` symbolic '"'

stringLiteral' :: Parser String
stringLiteral' = many (notChar '\'') `surroundedBy` symbolic '\''

comma :: Parser Char
comma = symbolic ','

dot :: Parser Char
dot = symbolic '.'

colon :: Parser Char
colon = symbolic ':'

semicolon :: Parser Char
semicolon = symbolic ';'

parens :: Parser a -> Parser a
parens = between (symbolic '(') (symbolic ')')

brackets :: Parser a -> Parser a
brackets = between (symbolic '[') (symbolic ']')

braces :: Parser a -> Parser a
braces = between (symbolic '{') (symbolic '}')

angles :: Parser a -> Parser a
angles = between (symbolic '<') (symbolic '>')

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

integerOrDouble :: Parser IntegerOrDouble
integerOrDouble = MkDouble <$> double <|> MkInteger <$> integer

naturalOrDouble :: Parser IntegerOrDouble
naturalOrDouble = MkDouble <$> double <|> MkInteger <$> natural

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
