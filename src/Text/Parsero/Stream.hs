{-# LANGUAGE FlexibleInstances #-}

module Text.Parsero.Stream
  ( Stream
  , cons
  , empty
  , elem
  , notElem
  , drop
  , dropWhile
  , head
  , tail
  , take
  , takeWhile
  ) where

import qualified Data.Text      as T
import qualified Data.Text.Lazy as TL
import           Prelude        (Bool(..), Char, Integral, (==))
import qualified Prelude        as P

class P.Eq s => Stream s where
  cons :: Char -> s -> s
  empty :: s
  elem :: Char -> s -> Bool
  notElem :: Char -> s -> Bool
  drop :: Integral a => a -> s -> s
  dropWhile :: (Char -> Bool) -> s -> s
  head :: s -> Char
  tail :: s -> s
  take :: Integral a => a -> s -> s
  takeWhile :: (Char -> Bool) -> s -> s

instance Stream [Char] where
  cons = (:)
  empty = ""
  elem = P.elem
  notElem = P.notElem
  drop n = P.drop (P.fromIntegral n)
  dropWhile = P.dropWhile
  head = P.head
  tail = P.tail
  take n = P.take (P.fromIntegral n)
  takeWhile = P.takeWhile

instance Stream T.Text where
  cons = T.cons
  empty = T.empty
  elem a = T.foldr (\x _ -> x == a) False
  notElem x xs = P.not (elem x xs)
  drop n = T.drop (P.fromIntegral n)
  dropWhile = T.dropWhile
  head = T.head
  tail = T.tail
  take n = T.take (P.fromIntegral n)
  takeWhile = T.takeWhile

instance Stream TL.Text where
  cons = TL.cons
  empty = TL.empty
  elem a = TL.foldr (\x _ -> x == a) False
  notElem x xs = P.not (elem x xs)
  drop n = TL.drop (P.fromIntegral n)
  dropWhile = TL.dropWhile
  head = TL.head
  tail = TL.tail
  take n = TL.take (P.fromIntegral n)
  takeWhile = TL.takeWhile
