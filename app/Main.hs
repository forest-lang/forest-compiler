module Main where

import Lib
import System.Environment
import Data.Maybe


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
          putStrLn $ show a ++ "\n" ++ printModule a ++ "\n" ++ printWasm a
        Left err -> putStrLn $ "Dun goofed" ++ show err
    Nothing -> putStrLn "please provide a file to compile"

