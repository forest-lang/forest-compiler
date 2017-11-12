module Lib
    ( printWasm, printExpression, parseExpressionFromString, Expression(..), Operator(..)
    ) where

import Data.Char
import Data.Functor.Identity
import Text.Parsec

data Operator
  = Add
  | Subtract
  | Divide
  | Multiply
  deriving (Show, Eq)

data Expression
  = Identifier String
  | Number Int
  | Assignment String
               [String]
               Expression
  | Infix Operator
          Expression
          Expression
  deriving (Show, Eq)

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
  arguments <- many parseArgument
  char '='
  spaces
  value <- parseExpression
  return $ Assignment name arguments value
  where
    parseArgument = do
      name <- many1 letter
      spaces
      return name

parseExpression =
  try parseInfix <|> parseDigit <|> try parseDeclaration <|> parseIdentifier

parseIdentifier = do
  name <- many1 letter
  return $ Identifier name

parseString :: ParsecT [Char] u Identity Expression
parseString = do
  expr <- parseExpression
  eof
  return expr

parseExpressionFromString :: String -> Either ParseError Expression
parseExpressionFromString = parse parseString ""

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
      unwords [printExpression expr, operatorToString op, printExpression expr2]
    Assignment name args expr ->
      name ++ " " ++ unwords args ++ " = " ++ printExpression expr
    Identifier name -> name

indent :: String -> Int -> String
indent str level =
  unlines $ map (\line -> replicate level ' ' ++ line) (lines str)

printWasm :: Expression -> String
printWasm expr = "(module\n" ++ indent (printWasmExpr expr) 2 ++ ")"
  where
    printWasmExpr expr =
      case expr of
        Number n -> "(i32.const " ++ show n ++ ")"
        Assignment name args expr ->
          "(export \"" ++
          name ++
          "\" (func $" ++
          name ++
          "))\n(func $" ++
          name ++
          " " ++
          paramsString args ++
          " " ++ "(result i32)\n" ++ indent ("(return \n" ++ indent (printWasmExpr expr) 2 ++ ")") 2 ++ ")"
        Infix op expr expr2 ->
          "(" ++
          opString op ++
          "\n" ++ indent (printWasmExpr expr ++ "\n" ++ printWasmExpr expr2) 2 ++ ")\n"
        Identifier name -> "(get_local $" ++ name ++ ")\n"
    paramsString args = unwords (paramString <$> args)
    paramString arg = "(param $" ++ arg ++ " i32)"
    opString op =
      case op of
        Add -> "i32.add"
        Subtract -> "i32.subtract"
        Multiply -> "i32.multiply"
        Divide -> "i32.divide"
