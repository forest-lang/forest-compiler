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
    ["build", filename] -> build compile filename
    ["format", filename] -> build format filename
    ["check", filename] -> do
      contents <- readFile filename
      putStrLn $
        case check contents of
          Success () -> "Compiled successfully"
          ParseErr err -> parseErrorPretty err
          CompileErr errors -> unlines . toList $ printError <$> errors
    _ -> putStrLn "please provide a file to compile"
  where
    build f filename = do
      contents <- readFile filename
      case f contents of
        Right a -> putStrLn a
        Left err ->
          putStrLn $
          "Syntax error in " ++
          filename ++ "\n" ++ parseErrorPretty' contents err
    printError (CompileError error) = error
