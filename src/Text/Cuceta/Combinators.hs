module Text.Cuceta.Combinators
  ( between
  , choice
  , item
  , noneOf
  , oneOf
  , option
  , satisfy
  , skipMany
  , skipOptional
  , skipSome
  , skipWhile
  , surroundedBy
  , try
  ) where

import Text.Cuceta.Parser

item :: Parser Char
item = MkParser $ \input ->
  case input of
    [] -> (Left EmptyInput, [])
    (a:rest) -> (Right a, rest)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = MkParser $ \input ->
  case input of
    [] -> (Left EndOfStream, [])
    (a:rest) ->
      if p a then (Right a, rest)
      else (Left DoesNotSatisfy, rest)

try :: Parser a -> Parser a
try p = MkParser $ \input ->
  case parse p input of
    (Left err, _) -> (Left err, input)
    (Right a, input') -> (Right a, input')

oneOf :: [Char] -> Parser Char
oneOf xs = satisfy (\x -> x `elem` xs)

noneOf :: [Char] -> Parser Char
noneOf xs = satisfy (\x -> x `notElem` xs)

skipMany :: Parser a -> Parser ()
skipMany p = many p *> pure ()

skipSome :: Parser a -> Parser ()
skipSome p = some p *> pure ()

skipOptional :: Parser a -> Parser ()
skipOptional p = () <$ p <|> pure ()

skipWhile :: (Char -> Bool) -> Parser ()
skipWhile = skipMany . satisfy

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = do
  open
  val <- p
  close
  pure val

choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty

option :: a -> Parser a -> Parser a
option a p = p <|> pure a

surroundedBy :: Parser a -> Parser s -> Parser a
surroundedBy p s = between s s p
