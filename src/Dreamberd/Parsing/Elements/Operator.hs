{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Dreamberd.Parsing.Elements.Operator (parseOperator) where


import Dreamberd.Types (AstNode(..))

parseOperator :: String -> Either String (AstNode -> AstNode -> AstNode, String)
parseOperator ('<':'=':xs) = Right (\x y -> AstNodeBool (x <= y), xs)
parseOperator ('>':'=':xs) = Right (\x y -> AstNodeBool (x >= y), xs)
parseOperator ('<':xs)     = Right (\x y -> AstNodeBool (x < y), xs)
parseOperator ('>':xs)     = Right (\x y -> AstNodeBool (x > y), xs)
parseOperator ('=':'=':xs) = Right (\x y -> AstNodeBool (x == y), xs)
parseOperator xs           = Left $ "Unknown operator: " ++ take 10 xs
