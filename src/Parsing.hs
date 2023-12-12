{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Parsing (
    parse,
    Parser,
    parseChar,
    parseAnyChar,
    parseOr,
    parseAnd,
    parseAndWith,
    parseMany,
    parseSome,
    parseUInt,
    parseInt,
    parsePair,
    parseList
    ) where

import Control.Applicative (Alternative(..))
import Data.Char (isDigit)

data Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input ->
    case p input of
      Just (result, rest) -> Just (f result, rest)
      Nothing -> Nothing

instance Applicative Parser where
  pure x = Parser $ \input -> Just (x, input)
  (Parser p1) <*> (Parser p2) = Parser $ \input ->
    case p1 input of
      Just (f, rest) -> case p2 rest of
        Just (result, rest') -> Just (f result, rest')
        Nothing -> Nothing
      Nothing -> Nothing

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input ->
    case p1 input of
      Just (result, rest) -> Just (result, rest)
      Nothing -> p2 input

instance Monad Parser where
  return = pure
  (Parser p) >>= f = Parser $ \input ->
    case p input of
      Just (result, rest) -> parse (f result) rest
      Nothing -> Nothing



parseChar :: Char -> Parser Char
parseChar c = Parser f
  where f (x:xs)
          | x == c = Just (c, xs)
          | otherwise = Nothing
        f _ = Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar = foldr ((<|>) . parseChar) empty

parseOr :: Parser a -> Parser a -> Parser a
parseOr = (<|>)

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 = (,) <$> p1 <*> p2

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f p1 p2 = f <$> p1 <*> p2


parseMany :: Parser a -> Parser [a]
parseMany p = Parser $ \input ->
  case parse (parseSome p) input of
    Just (results, rest) -> Just (results, rest)
    Nothing -> Just ([], input)

parseSome :: Parser a -> Parser [a]
parseSome p = Parser $ \input ->
  case parse p input of
    Just (result, rest) ->
      case parse (parseMany p) rest of
        Just (results, rest') -> Just (result : results, rest')
        Nothing -> Just ([result], rest)
    Nothing -> Nothing



parseUInt :: Parser Int
parseUInt = Parser f
  where
    f input = case span isDigit input of
                ("", _) -> Nothing
                (numStr, rest) -> Just (read numStr, rest)


parseInt :: Parser Int
parseInt = parseNeg <|> parseUInt
  where
    parseNeg = parseChar '-' *> (negate <$> parseUInt)


parsePair :: Parser a -> Parser (a, a)
parsePair p = parseChar '(' *> parseAndWith (,) p (parseChar ' ' *> p) <* parseChar ')'


parseList :: Parser a -> Parser [a]
parseList p = parseChar '(' *> parseList' p <* parseChar ')'
  where
    parseList' p = parseAndWith (:) p (parseChar ' ' *> parseList' p) <|> pure []
