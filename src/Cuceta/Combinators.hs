module Cuceta.Combinators
  ( between
  , item
  , noneOf
  , oneOf
  , satisfy
  , skipMany
  , skipOptional
  , skipSome
  ) where

import Cuceta.Parser

item :: Parser Char
item = MkParser $ \input ->
  case input of
    [] -> []
    (x:xs) -> [(x, xs)]

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = MkParser $ \input ->
  case input of
    [] -> []
    (x:xs) -> if p x then [(x, xs)] else []

oneOf :: [Char] -> Parser Char
oneOf = satisfy . flip elem

noneOf :: [Char] -> Parser Char
noneOf = satisfy . flip notElem

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = do
  open
  val <- p
  close
  pure val

skipMany :: Parser a -> Parser ()
skipMany p = many p *> pure ()

skipSome :: Parser a -> Parser ()
skipSome p = some p *> pure ()

skipOptional :: Parser a -> Parser ()
skipOptional p = () <$ p <|> pure ()
