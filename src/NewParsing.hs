{-# LANGUAGE ViewPatterns #-}

module NewParsing (parseDreamberd) where

import NewTypes (AstNode (Number, Operator, String))

import Data.List (stripPrefix)

parseElement :: String -> [AstNode] -> Maybe (String, [AstNode])
parseElement (stripPrefix "var" -> Just restCode) ast = Just (restCode, ast ++ [Operator "=" (String "a") (Number 42)])
parseElement _ _ = Nothing

parseDreamberd :: String -> [AstNode] -> Maybe [AstNode]
parseDreamberd sourceCode ast = parseElement sourceCode ast >>= uncurry parseDreamberd

-- TODO: add condition to stop when no more source code
-- donc le premier call a parseDreamberd sera avec tout le source code et une liste vide, il va se call lui même récursivement
-- et à la fin il va se stop quand y'a plus de source code (ou erreur, à gérer) et renvoyer l'AST complet
