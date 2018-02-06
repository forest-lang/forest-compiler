module Main where

import Compiler
import Data.Maybe
import HaskellSyntax
import qualified WASM as W
import System.Environment
import Text.Megaparsec.Error

head' :: [a] -> Maybe a
head' list =
  case length list of
    0 -> Nothing
    _ -> Just $ head list

main :: IO ()
main = do
  args <- getArgs
  let maybeFilename = head' args
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
