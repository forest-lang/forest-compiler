{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.List.NonEmpty (toList)
import Data.Maybe
import Safe
import System.Environment
import Text.Megaparsec.Error

import Compiler
import HaskellSyntax
import TypeChecker

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["build", filename] -> do
      contents <- readFile filename
      putStrLn $
        case compile contents of
          Success w -> w
          ParseErr err -> reportParseError filename contents err
          CompileErr errors -> unlines . toList $ printError <$> errors
    ["format", filename] -> build format filename
    ["check", filename] -> do
      contents <- readFile filename
      putStrLn $
        case check contents of
          Success _ -> "Compiled successfully"
          ParseErr err -> reportParseError filename contents err
          CompileErr errors -> unlines . toList $ printError <$> errors
    _ -> putStrLn "please provide a file to compile"
  where
    build f filename = do
      contents <- readFile filename
      case f contents of
        Right a -> putStrLn a
        Left err -> putStrLn $ reportParseError filename contents err
    printError (CompileError error) = error

reportParseError :: String -> String -> ParseError' -> String
reportParseError filename contents err =
  "Syntax error in " ++ filename ++ "\n" ++ parseErrorPretty' contents err
