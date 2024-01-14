{-# LANGUAGE LambdaCase #-}

module Dreamberd.Compilation.Preprocessing (
    executePreprocessing,
) where

import Data.List (elemIndex, intercalate, partition)
import Dreamberd.Parser (parseDreamberd)
import Dreamberd.Types (AstNode (..), File (..))
import System.Directory (getCurrentDirectory, getHomeDirectory)

executePreprocessing :: File [AstNode] -> IO (Either String [AstNode])
executePreprocessing file =
    preprocessImports file >>= \case
        Right nodes' -> return $ Right $ preprocessFunctions nodes'
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
hoistFunctions (If cond trueBody falseBody) = If cond (preprocessFunctions trueBody) (preprocessFunctions falseBody)
hoistFunctions (Loop test body initNode updateNode) = Loop test (preprocessFunctions body) initNode updateNode
hoistFunctions a = a
