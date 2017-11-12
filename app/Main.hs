module Main where

import Lib
import Text.Parsec


main :: IO ()
main = do
  let result = parseExpressionFromString "addOne n = 5 + n"
  case result of
    Right a ->
      putStrLn $ show a ++ "\n" ++ printExpression a ++ "\n" ++ printWasm a
    Left err -> putStrLn $ "Dun goofed" ++ show err
