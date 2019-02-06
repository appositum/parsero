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
import           Text.Parsero.Combinators
import           Text.Parsero.Parser
import           Text.Parsero.Stream      (Stream)
import qualified Text.Parsero.Stream      as S

char :: Stream s => Char -> Parsero s Char
char c = satisfy (==c)

char' :: Stream s => Char -> Parsero s Char
char' c =  char (toLower c)
       <|> char (toUpper c)
       <|> char (toTitle c)

anyChar :: Stream s => Parsero s Char
anyChar = satisfy (const True)

notChar :: Stream s => Char -> Parsero s Char
notChar c = satisfy (/=c)

space :: Stream s => Parsero s Char
space = satisfy isSpace

skipWhitespaces :: Stream s => Parsero s ()
skipWhitespaces = skipMany space

lower :: Stream s => Parsero s Char
lower = satisfy isLower

upper :: Stream s => Parsero s Char
upper = satisfy isUpper

alphaNum :: Stream s => Parsero s Char
alphaNum = satisfy isAlphaNum

alpha :: Stream s => Parsero s Char
alpha = satisfy isAlpha

letter :: Stream s => Parsero s Char
letter = alpha

chunk :: Stream s => s -> Parsero s s
chunk css
  | css == S.empty = pure S.empty
  | otherwise = char c *> chunk cs *> pure css
  where cs = S.tail css
        c = S.head css

string :: Stream s => s -> Parsero s s
string = chunk

digit :: Stream s => Parsero s Char
digit = satisfy isDigit

tab :: Stream s => Parsero s Char
tab = char '\t'

newline :: Stream s => Parsero s Char
newline = char '\n'

carriage :: Stream s => Parsero s Char
carriage = char '\r'

crlf :: Stream s => Parsero s Char
crlf = carriage *> newline

eol :: Stream s => Parsero s Char
eol = newline <|> crlf
