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
  let maybeFilename = headMay args
  case maybeFilename of
    Just filename -> do
      contents <- readFile filename

      case compile contents of
        Right a -> putStrLn a
        Left err ->
          putStrLn $
          "Syntax error in " ++
          filename ++ "\n" ++ parseErrorPretty' contents err
    Nothing -> putStrLn "please provide a file to compile"
