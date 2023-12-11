{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
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

import Control.Applicative (Alternative(..), many, some)
import Data.Char (isDigit, isAlpha)

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
  case parse p input of
    Just (result, rest) ->
      case parse (parseMany p) rest of
        Just (results, rest') -> Just (result : results, rest')
        Nothing -> Just ([result], rest)
    Nothing -> Nothing

parseSome :: Parser a -> Parser [a]
parseSome p = Parser $ \input ->
    case parse p input of
        Just (result, rest)
            | rest /= input -> Nothing
            | otherwise -> Just ([], input)
            | otherwise -> Nothing
        Nothing -> Nothing



parseUInt :: Parser Int
parseUInt = Parser f
  where f input = case parse (parseSome (parseChar '0' <|> parseChar '1' <|> parseChar '2' <|> parseChar '3' <|> parseChar '4' <|> parseChar '5' <|> parseChar '6' <|> parseChar '7' <|> parseChar '8' <|> parseChar '9')) input of
          Just (result, rest) -> Just (read result, rest)
          Nothing -> Nothing

parseInt :: Parser Int
parseInt = Parser f
  where f input = case parse (parseChar '-') input of
          Just (_, rest) -> case parse parseUInt rest of
            Just (result, rest') -> Just (-result, rest')
            Just (result, rest) -> Just (result, rest)
            Nothing -> Nothing

parsePair :: Parser a -> Parser (a, a)
parsePair p = parseAndWith (,) p p

parseList :: Parser a -> Parser [a]
parseList l = parseAndWith (:) l (parseMany (parseAndWith (,) (parseChar ',') l))