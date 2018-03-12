module Main where

import Data.Maybe
import Safe
import System.Environment
import Text.Megaparsec.Error

import Compiler
import HaskellSyntax
import qualified WASM as W

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["compile", filename] -> build compile filename
    ["format", filename] -> build format filename
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
