module Text.Cuceta.Combinators
  ( between
  , choice
  , consume
  , consumeMany
  , consumeSome
  , consumeTill
  , item
  , manyTill
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

manyTill :: Alternative f => f a -> f end -> f [a]
manyTill p end = go where
  go =  ([] <$ end)
    <|> ((:) <$> p <*> go)

consume :: Int -> Parser [Char]
consume n = MkParser $ \input ->
  case input of
    [] -> (Left EndOfStream, [])
    xs -> (Right (take n xs), drop n xs)

consumeMany :: (Char -> Bool) -> Parser [Char]
consumeMany p = MkParser $ \input ->
  case input of
    [] -> (Right [], [])
    xs -> (Right (takeWhile p xs), dropWhile p xs)

consumeSome :: (Char -> Bool) -> Parser [Char]
consumeSome p = MkParser $ \input ->
  case input of
    [] -> (Left EndOfStream, [])
    xs -> (Right (takeWhile p xs), dropWhile p xs)

consumeTill :: (Char -> Bool) -> Parser [Char]
consumeTill p = consumeMany (not . p)

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
