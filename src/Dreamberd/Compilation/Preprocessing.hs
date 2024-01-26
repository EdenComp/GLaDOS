{-# LANGUAGE LambdaCase #-}

module Dreamberd.Compilation.Preprocessing (
    executePreprocessing,
) where

import Data.Fixed (mod')
import Data.List (elemIndex, intercalate, partition)
import Dreamberd.Parser (parseDreamberd)
import Dreamberd.Types (AstNode (..), File (..))
import System.Directory (getCurrentDirectory, getHomeDirectory)

executePreprocessing :: File [AstNode] -> IO (Either String [AstNode])
executePreprocessing file =
    preprocessImports file >>= \case
        Right nodes' -> return $ Right $ preprocessOptimizations $ preprocessFunctions nodes'
        Left err -> return $ Left err

preprocessImports :: File [AstNode] -> IO (Either String [AstNode])
preprocessImports (File filename nodes) = do
    current <- getCurrentDirectory
    let path = if head filename == '/' then filename else current ++ "/" ++ filename
    preprocessNodes nodes path [sanitizePath path] >>= \case
        Left err -> return $ Left err
        Right (_, nodes') -> return $ Right nodes'

preprocessNodes :: [AstNode] -> String -> [String] -> IO (Either String ([String], [AstNode]))
preprocessNodes [] _ imports = return $ Right (imports, [])
preprocessNodes (Import path : rest) filename imports = do
    preprocessImport path filename imports >>= \case
        Right (imports', nodes) ->
            preprocessNodes rest filename imports' >>= \case
                Right (imports'', nodes') -> return $ Right (imports'', nodes ++ nodes')
                Left err -> return $ Left err
        Left err -> return $ Left err
preprocessNodes (x : xs) filename imports =
    preprocessNodes xs filename imports >>= \case
        Right (imports', nodes) -> return $ Right (imports', x : nodes)
        Left err -> return $ Left err

preprocessImport :: String -> String -> [String] -> IO (Either String ([String], [AstNode]))
preprocessImport lib filename imports = case getImportPackage lib "" of
    Just (package, lib') -> do
        home <- getHomeDirectory
        file <- resolveImport (home ++ "/.dreamberd/" ++ package ++ "/" ++ resolvePath lib') imports
        case file of
            Nothing -> return $ Right (imports, [])
            Just (imports', file') -> importFile (home ++ "/.dreamberd/" ++ package ++ "/" ++ resolvePath lib') (imports', file')
    Nothing -> do
        file <- resolveImport path imports
        case file of
            Nothing -> return $ Right (imports, [])
            Just (imports', file') -> importFile path (imports', file')
      where
        path = getFileDirectory filename ++ resolvePath lib

getImportPackage :: String -> String -> Maybe (String, String)
getImportPackage [] _ = Nothing
getImportPackage (':' : xs) package = Just (package, xs)
getImportPackage (x : xs) package = getImportPackage xs (package ++ [x])

resolvePath :: String -> FilePath
resolvePath [] = ".db4"
resolvePath ['.', 'd', 'b', '4'] = ".db4"
resolvePath (x : xs) = x : resolvePath xs

getFileDirectory :: FilePath -> FilePath
getFileDirectory = reverse . dropWhile (/= '/') . reverse

sanitizePath :: String -> FilePath
sanitizePath path = sanitizeDoubleSlashes $ sanitizeDoubleDots $ sanitizeSimpleDots path

sanitizeSimpleDots :: String -> FilePath
sanitizeSimpleDots [] = []
sanitizeSimpleDots ['/', '.'] = "/"
sanitizeSimpleDots ('/' : '.' : '/' : xs) = sanitizeSimpleDots ('/' : xs)
sanitizeSimpleDots (x : xs) = x : sanitizeSimpleDots xs

sanitizeDoubleDots :: String -> FilePath
sanitizeDoubleDots [] = []
sanitizeDoubleDots path = case elemIndex ".." folders of
    Just idx -> sanitizeDoubleDots $ '/' : intercalate "/" (take (idx - 1) folders ++ drop (idx + 1) folders)
    Nothing -> path
  where
    folders = words [if c == '/' then ' ' else c | c <- path]

sanitizeDoubleSlashes :: String -> FilePath
sanitizeDoubleSlashes [] = []
sanitizeDoubleSlashes ('/' : '/' : xs) = sanitizeDoubleSlashes ('/' : xs)
sanitizeDoubleSlashes (x : xs) = x : sanitizeDoubleSlashes xs

resolveImport :: FilePath -> [String] -> IO (Maybe ([String], String))
resolveImport path imports = do
    file <- readFile path
    if sanitizePath path `elem` imports
        then return Nothing
        else return $ Just (sanitizePath path : imports, file)

importFile :: String -> ([String], String) -> IO (Either String ([String], [AstNode]))
importFile lib (imports, code) = case parseDreamberd (File lib code) of
    Right ast -> preprocessNodes ast lib imports
    Left err -> return $ Left ("Error while importing " ++ lib ++ ": " ++ err)

preprocessFunctions :: [AstNode] -> [AstNode]
preprocessFunctions a = map hoistFunctions functions ++ map hoistFunctions rest
  where
    (functions, rest) = partition isFunction a
    isFunction :: AstNode -> Bool
    isFunction Function{} = True
    isFunction _ = False

hoistFunctions :: AstNode -> AstNode
hoistFunctions (Function name args ast) = Function name args (preprocessFunctions ast)
hoistFunctions (Lambda args ast) = Lambda args (preprocessFunctions ast)
hoistFunctions (If cond trueBody falseBody) = If cond (preprocessFunctions trueBody) (preprocessFunctions falseBody)
hoistFunctions (Loop test body initNode updateNode) = Loop test (preprocessFunctions body) initNode updateNode
hoistFunctions (Scope ast) = Scope $ preprocessFunctions ast
hoistFunctions a = a

preprocessOptimizations :: [AstNode] -> [AstNode]
preprocessOptimizations (Call name insts : xs) = optimizeNode (Call name $ preprocessOptimizations insts) : preprocessOptimizations xs
preprocessOptimizations (Function name args ast : xs) = Function name args (preprocessOptimizations ast) : preprocessOptimizations xs
preprocessOptimizations (Lambda args ast : xs) = Lambda args (preprocessOptimizations ast) : xs
preprocessOptimizations (If (Call name insts) trueBody falseBody : xs) = optimizeNode (If (optimizeNode $ Call name $ preprocessOptimizations insts) (preprocessOptimizations trueBody) (preprocessOptimizations falseBody)) : preprocessOptimizations xs
preprocessOptimizations (If cond trueBody falseBody : xs) = optimizeNode (If cond (preprocessOptimizations trueBody) (preprocessOptimizations falseBody)) : preprocessOptimizations xs
preprocessOptimizations (Loop (Call name insts) body initNode updateNode : xs) = optimizeNode (Loop (optimizeNode $ Call name $ preprocessOptimizations insts) (preprocessOptimizations body) initNode updateNode) : preprocessOptimizations xs
preprocessOptimizations (Loop cond body initNode updateNode : xs) = optimizeNode (Loop cond (preprocessOptimizations body) initNode updateNode) : preprocessOptimizations xs
preprocessOptimizations (Scope ast : xs) = Scope (preprocessOptimizations ast) : preprocessOptimizations xs
preprocessOptimizations (x : xs) = x : preprocessOptimizations xs
preprocessOptimizations [] = []

optimizeNode :: AstNode -> AstNode
optimizeNode (Call (Identifier name) [Integer x, Integer y]) = optimizeIntegerCall name x y
optimizeNode (Call (Identifier name) [Float x, Float y]) = optimizeFloatCall name x y
optimizeNode (Call (Identifier name) [Boolean x, Boolean y]) = optimizeBoolCall name x y
optimizeNode (Call (Identifier name) [String x, String y]) = optimizeStringCall name x y
optimizeNode (Call (Identifier name) [x, y]) = Call (Identifier name) [optimizeNode x, optimizeNode y]
optimizeNode (If cond trueBody falseBody) = case astToBool cond of
    Just True -> optimizeNode $ Scope trueBody
    Just False -> optimizeNode $ Scope falseBody
    Nothing -> If cond trueBody falseBody
optimizeNode (Loop cond body initNode updateNode) = case astToBool cond of
    Just False -> optimizeNode $ Scope []
    _ -> Loop cond body initNode updateNode
optimizeNode x = x

optimizeIntegerCall :: String -> Int -> Int -> AstNode
optimizeIntegerCall name x y = case name of
    "+" -> Integer (x + y)
    "-" -> Integer (x - y)
    "*" -> Integer (x * y)
    "/" -> Integer (x `div` y)
    "%" -> Integer (x `mod` y)
    "^" -> Integer (x ^ y)
    "==" -> Boolean (x == y)
    "!=" -> Boolean (x /= y)
    "<" -> Boolean (x < y)
    ">" -> Boolean (x > y)
    "<=" -> Boolean (x <= y)
    ">=" -> Boolean (x >= y)
    _ -> Call (Identifier name) [Integer x, Integer y]

optimizeFloatCall :: String -> Double -> Double -> AstNode
optimizeFloatCall name x y = case name of
    "+" -> Float (x + y)
    "-" -> Float (x - y)
    "*" -> Float (x * y)
    "/" -> Float (x / y)
    "%" -> Float (mod' x y)
    "^" -> Float (x ** y)
    "==" -> Boolean (x == y)
    "!=" -> Boolean (x /= y)
    "<" -> Boolean (x < y)
    ">" -> Boolean (x > y)
    "<=" -> Boolean (x <= y)
    ">=" -> Boolean (x >= y)
    _ -> Call (Identifier name) [Float x, Float y]

optimizeBoolCall :: String -> Bool -> Bool -> AstNode
optimizeBoolCall name x y = case name of
    "&&" -> Boolean (x && y)
    "||" -> Boolean (x || y)
    "==" -> Boolean (x == y)
    "!=" -> Boolean (x /= y)
    "^" -> Boolean (x /= y)
    _ -> Call (Identifier name) [Boolean x, Boolean y]

optimizeStringCall :: String -> String -> String -> AstNode
optimizeStringCall name x y = case name of
    "+" -> String (x ++ y)
    "==" -> Boolean (x == y)
    "!=" -> Boolean (x /= y)
    _ -> Call (Identifier name) [String x, String y]

astToBool :: AstNode -> Maybe Bool
astToBool (Boolean x) = Just x
astToBool (Integer 0) = Just False
astToBool (Integer _) = Just True
astToBool (Float 0) = Just False
astToBool (Float _) = Just True
astToBool (String "") = Just False
astToBool (String _) = Just True
astToBool _ = Nothing