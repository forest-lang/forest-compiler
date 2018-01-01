module Main where

import Lib
import System.Environment
import Data.Maybe
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

      let result = parseExpressionFromString contents

      case result of
        Right a ->
          putStrLn $ printWasm a
        Left err -> putStrLn $ "Syntax error in " ++ filename ++ "\n" ++ parseErrorPretty' contents err
    Nothing -> putStrLn "please provide a file to compile"

