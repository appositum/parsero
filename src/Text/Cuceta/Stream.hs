module Text.Cuceta.Stream
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

import qualified Data.Text as T
import           Prelude   (Bool(..), Char, Eq, Int, String, (==))
import qualified Prelude   as P

class Eq s => Stream s where
  cons :: Char -> s -> s
  empty :: s
  elem :: Char -> s -> Bool
  notElem :: Char -> s -> Bool
  drop :: Int -> s -> s
  dropWhile :: (Char -> Bool) -> s -> s
  head :: s -> Char
  tail :: s -> s
  take :: Int -> s -> s
  takeWhile :: (Char -> Bool) -> s -> s

instance Stream String where
  cons = (:)
  empty = ""
  elem = P.elem
  notElem = P.notElem
  drop = P.drop
  dropWhile = P.dropWhile
  head = P.head
  tail = P.tail
  take = P.take
  takeWhile = P.takeWhile

instance Stream T.Text where
  cons = T.cons
  empty = T.empty
  elem a = T.foldr (\x _ -> x == a) False
  notElem x xs = P.not (elem x xs)
  drop = T.drop
  dropWhile = T.dropWhile
  head = T.head
  tail = T.tail
  take = T.take
  takeWhile = T.takeWhile
