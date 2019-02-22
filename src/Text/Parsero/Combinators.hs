{-# LANGUAGE LambdaCase #-}

module Text.Parsero.Combinators
  ( between
  , choice
  , consume
  , consumeMany
  , consumeSome
  , consumeTill
  , endBy
  , endBy1
  , manyTill
  , noneOf
  , oneOf
  , option
  , satisfy
  , skipMany
  , skipOptional
  , skipSome
  , skipWhile
  , sepBy
  , sepBy1
  , surroundedBy
  , try
  ) where

import           Control.Applicative (empty)
import           Text.Parsero.Parser
import           Text.Parsero.Stream (Stream)
import qualified Text.Parsero.Stream as S

satisfy :: Stream s => (Char -> Bool) -> Parsero s Char
satisfy p = MkParsero $ \case
  input | input == S.empty -> (Left EndOfStream, S.empty)
        | otherwise -> if p a then (Right a, rest) else (Left DoesNotSatisfy, rest)
        where rest = S.tail input
              a = S.head input

try :: Stream s => Parsero s a -> Parsero s a
try p = MkParsero $ \input ->
  case parse p input of
    (Left err, _) -> (Left err, input)
    (Right a, input') -> (Right a, input')

oneOf :: Stream s => [Char] -> Parsero s Char
oneOf xs = satisfy (\x -> x `elem` xs)

noneOf :: Stream s => [Char] -> Parsero s Char
noneOf xs = satisfy (\x -> x `notElem` xs)

skipMany :: Stream s => Parsero s a -> Parsero s ()
skipMany p = many p *> pure ()

skipSome :: Stream s => Parsero s a -> Parsero s ()
skipSome p = some p *> pure ()

skipOptional :: Stream s => Parsero s a -> Parsero s ()
skipOptional p = () <$ p <|> pure ()

skipWhile :: Stream s => (Char -> Bool) -> Parsero s ()
skipWhile = skipMany . satisfy

manyTill :: Stream s => Parsero s a -> Parsero s end -> Parsero s [a]
manyTill p end = go where
  go = ([] <$ end) <|> ((:) <$> p <*> go)

consume :: Stream s => Int -> Parsero s s
consume n = MkParsero $ \case
  input | input == S.empty -> (Left EndOfStream, S.empty)
        | otherwise -> (Right x, xs)
        where xs = S.drop n input
              x = S.take n input

consumeMany :: Stream s => (Char -> Bool) -> Parsero s s
consumeMany p = MkParsero $ \case
  input | input == S.empty -> (Right S.empty, S.empty)
        | otherwise -> (Right x, xs)
        where xs = S.dropWhile p input
              x = S.takeWhile p input

consumeSome :: Stream s => (Char -> Bool) -> Parsero s s
consumeSome p = MkParsero $ \case
  input | input == S.empty -> (Left EndOfStream, S.empty)
        | otherwise -> (Right x, xs)
        where xs = S.dropWhile p input
              x = S.takeWhile p input

consumeTill :: Stream s => (Char -> Bool) -> Parsero s s
consumeTill p = consumeMany (not . p)

between :: Stream s => Parsero s open -> Parsero s close
        -> Parsero s a -> Parsero s a
between open close p = open *> p <* close

choice :: Stream s => [Parsero s a] -> Parsero s a
choice = foldr (<|>) empty

option :: Stream s => a -> Parsero s a -> Parsero s a
option a p = p <|> pure a

surroundedBy :: Stream s => Parsero s a -> Parsero s sur -> Parsero s a
surroundedBy p s = between s s p

sepBy :: Stream s => Parsero s a -> Parsero s sep -> Parsero s [a]
sepBy p sep = sepBy1 p sep <|> pure []

sepBy1 :: Stream s => Parsero s a -> Parsero s sep -> Parsero s [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

endBy :: Stream s => Parsero s a -> Parsero s end -> Parsero s [a]
endBy p end = many (p <* end)

endBy1 :: Stream s => Parsero s a -> Parsero s end -> Parsero s [a]
endBy1 p end = some (p <* end)
