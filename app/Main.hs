module Main where

import Data.Char
import Data.Functor.Identity
import Lib
import Text.Parsec

data Operator
  = Add
  | Subtract
  | Divide
  | Multiply
  deriving (Show)

data Expression
  = Number Int
  | Assignment String
               Expression
  | Infix Operator
          Expression
          Expression
  deriving (Show)

parseDigit = do
  value <- many1 digit
  return $ Number (read value)

parseOperator = do
  char <- oneOf "+-*/"
  return $
    case char of
      '+' -> Add
      '-' -> Subtract
      '*' -> Multiply
      '/' -> Divide

parseInfix = do
  a <- parseDigit
  spaces
  operator <- parseOperator
  spaces
  b <- parseExpression
  return $ Infix operator a b

parseDeclaration = do
  name <- many letter
  spaces
  char '='
  spaces
  value <- parseExpression
  return $ Assignment name value

parseExpression = try parseInfix <|> parseDigit <|> parseDeclaration

parseString = do
  expr <- parseExpression
  eof
  return expr

operatorToString :: Operator -> String
operatorToString op =
  case op of
    Add -> "+"
    Subtract -> "-"
    Multiply -> "*"
    Divide -> "/"

printExpression :: Expression -> String
printExpression expr =
  case expr of
    Number n -> show n
    Infix op expr expr2 ->
      printExpression expr ++
      " " ++ operatorToString op ++ " " ++ printExpression expr2
    Assignment name expr -> name ++ " = " ++ printExpression expr

main :: IO ()
main = do
  let result = parse parseString "" "x = 5 + 3 + 2"
  case result of
    Right a -> putStrLn $ show a ++ "\n" ++ printExpression a
    Left err -> putStrLn $ "Dun goofed" ++ show err
