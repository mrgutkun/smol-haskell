module Dyck where

import Data.Bifunctor (first)

data Parser stream result = 
  Parser { runParser :: stream -> [(result, stream)]}

instance Functor (Parser stream) where
  fmap f (Parser p) = Parser $ fmap (first f) . p

instance Applicative (Parser stream) where
  pure a = Parser $ \stream -> [(a, stream)]
  Parser pf <*> Parser pv = Parser $ \stream -> do
    (f, stream') <- pf stream
    (v, stream'') <- pv stream
    pure (f v, stream'')

instance Monad (Parser stream) where
  Parser pa >>= pf = Parser $ \stream -> do
    (a, stream') <- pa stream
    runParser (pf a) stream'


instance Alternative (Parser stream) where
  empty = Parser $ const []
  Parser left <|> Parser right = Parser \stream ->
    left stream <> right stream

data Dyck 
  = Open Int
  | Close Int

data AST = 
  | In Int [AST]

parse :: [Dyck] -> Maybe [AST]
parse = listToMaybe . runParser parser

parser :: Parser [Dyck] [AST]
parser = many do
  single <- char
  case single of
    Close _ -> empty
    Open n -> do
      internal <- many parser
      exact $ Close n
      pure $ In n internal

exact :: Eq item => item -> Parser [item] item
exact item = Parser $ \items -> 
  case items of
    head : tail | head == item -> [(item, tail)]
    _ -> []

char :: Parser [item] item
char = Parser $ \items -> case items of
  head : tail -> [(head, tail)]
  [] -> []
