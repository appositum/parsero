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

satisfy :: Stream s => (Char -> Bool) -> Parser s Char
satisfy p = MkParser $ \case
  input | input == S.empty -> (Left EndOfStream, S.empty)
        | otherwise -> if p a then (Right a, rest) else (Left DoesNotSatisfy, rest)
        where rest = S.tail input
              a = S.head input

try :: Stream s => Parser s a -> Parser s a
try p = MkParser $ \input ->
  case parse p input of
    (Left err, _) -> (Left err, input)
    (Right a, input') -> (Right a, input')

oneOf :: Stream s => [Char] -> Parser s Char
oneOf xs = satisfy (\x -> x `elem` xs)

noneOf :: Stream s => [Char] -> Parser s Char
noneOf xs = satisfy (\x -> x `notElem` xs)

skipMany :: Stream s => Parser s a -> Parser s ()
skipMany p = many p *> pure ()

skipSome :: Stream s => Parser s a -> Parser s ()
skipSome p = some p *> pure ()

skipOptional :: Stream s => Parser s a -> Parser s ()
skipOptional p = () <$ p <|> pure ()

skipWhile :: Stream s => (Char -> Bool) -> Parser s ()
skipWhile = skipMany . satisfy

manyTill :: Stream s => Parser s a -> Parser s end -> Parser s [a]
manyTill p end = go where
  go = ([] <$ end) <|> ((:) <$> p <*> go)

consume :: Stream s => Int -> Parser s s
consume n = MkParser $ \case
  input | input == S.empty -> (Left EndOfStream, S.empty)
        | otherwise -> (Right x, xs)
        where xs = S.drop n input
              x = S.take n input

consumeMany :: Stream s => (Char -> Bool) -> Parser s s
consumeMany p = MkParser $ \case
  input | input == S.empty -> (Right S.empty, S.empty)
        | otherwise -> (Right x, xs)
        where xs = S.dropWhile p input
              x = S.takeWhile p input

consumeSome :: Stream s => (Char -> Bool) -> Parser s s
consumeSome p = MkParser $ \case
  input | input == S.empty -> (Left EndOfStream, S.empty)
        | otherwise -> (Right x, xs)
        where xs = S.dropWhile p input
              x = S.takeWhile p input

consumeTill :: Stream s => (Char -> Bool) -> Parser s s
consumeTill p = consumeMany (not . p)

between :: Stream s => Parser s fst -> Parser s snd -> Parser s a -> Parser s a
between open close p = open *> p <* close

choice :: Stream s => [Parser s a] -> Parser s a
choice = foldr (<|>) empty

option :: Stream s => a -> Parser s a -> Parser s a
option a p = p <|> pure a

surroundedBy :: Stream s => Parser s a -> Parser s sur -> Parser s a
surroundedBy p s = between s s p

sepBy :: Stream s => Parser s a -> Parser s sep -> Parser s [a]
sepBy p sep = sepBy1 p sep <|> pure []

sepBy1 :: Stream s => Parser s a -> Parser s sep -> Parser s [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

endBy :: Stream s => Parser s a -> Parser s end -> Parser s [a]
endBy p end = many (p <* end)

endBy1 :: Stream s => Parser s a -> Parser s end -> Parser s [a]
endBy1 p end = some (p <* end)
