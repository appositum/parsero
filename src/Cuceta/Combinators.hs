module Cuceta.Combinators
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
  , surroundedBy
  ) where

import Cuceta.Parser
import Data.Foldable (asum)

item :: Parser Char
item = MkParser $ \input ->
  case input of
    [] -> (Left "empty input", [])
    (a:rest) -> (Right a, rest)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = MkParser $ \input ->
  case input of
    [] -> (Left "end of stream", [])
    (a:rest) ->
      if p a then (Right a, rest)
      else (Left "does not satisfy", rest)

oneOf :: [Char] -> Parser Char
oneOf = satisfy . flip elem

noneOf :: [Char] -> Parser Char
noneOf = satisfy . flip notElem

skipMany :: Parser a -> Parser ()
skipMany p = many p *> pure ()

skipSome :: Parser a -> Parser ()
skipSome p = some p *> pure ()

skipOptional :: Parser a -> Parser ()
skipOptional p = () <$ p <|> pure ()

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = do
  open
  val <- p
  close
  pure val

choice :: [Parser a] -> Parser a
choice = asum

option :: a -> Parser a -> Parser a
option a p = p <|> pure a

surroundedBy :: Parser a -> Parser s -> Parser a
surroundedBy p s = between s s p
