module Text.Parsero.Char
  ( alpha
  , alphaNum
  , anyChar
  , carriage
  , char
  , char'
  , chunk
  , crlf
  , digit
  , eol
  , letter
  , lower
  , newline
  , notChar
  , space
  , skipWhitespaces
  , string
  , tab
  , upper
  ) where

import           Data.Char
import           Data.Text               (Text)
import           Text.Parsero.Combinators
import           Text.Parsero.Parser
import           Text.Parsero.Stream      (Stream)
import qualified Text.Parsero.Stream      as S

char :: Stream s => Char -> Parser s Char
char c = satisfy (==c)

char' :: Stream s => Char -> Parser s Char
char' c =  char (toLower c)
       <|> char (toUpper c)
       <|> char (toTitle c)

anyChar :: Stream s => Parser s Char
anyChar = satisfy (const True)

notChar :: Stream s => Char -> Parser s Char
notChar c = satisfy (/=c)

space :: Stream s => Parser s Char
space = satisfy isSpace

skipWhitespaces :: Stream s => Parser s ()
skipWhitespaces = skipMany space

lower :: Stream s => Parser s Char
lower = satisfy isLower

upper :: Stream s => Parser s Char
upper = satisfy isUpper

alphaNum :: Stream s => Parser s Char
alphaNum = satisfy isAlphaNum

alpha :: Stream s => Parser s Char
alpha = satisfy isAlpha

letter :: Stream s => Parser s Char
letter = alpha

chunk :: Stream s => s -> Parser s s
chunk css
  | css == S.empty = pure S.empty
  | otherwise = char c *> chunk cs *> pure css
  where cs = S.tail css
        c = S.head css

string :: Stream s => s -> Parser s s
string = chunk

digit :: Stream s => Parser s Char
digit = satisfy isDigit

tab :: Stream s => Parser s Char
tab = char '\t'

newline :: Stream s => Parser s Char
newline = char '\n'

carriage :: Stream s => Parser s Char
carriage = char '\r'

crlf :: Stream s => Parser s Char
crlf = carriage *> newline

eol :: Stream s => Parser s Char
eol = newline <|> crlf
