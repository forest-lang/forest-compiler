module Lib
  ( printWasm
  , printExpression
  , parseExpressionFromString
  , Expression(..)
  , Operator(..)
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

type Ident = String

ident :: String -> Ident
ident s = s

data Expression
  = Identifier Ident
  | Number Int
  | Assignment Ident
               [Ident]
               Expression
  | Infix Operator
          Expression
          Expression
  | Call Ident
         [Expression]
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
  return $ Assignment (ident name) arguments value
  where
    parseArgument = do
      name <- many1 letter
      spaces
      return (ident name)

parseCall = do
  name <- parseIdent
  spaces
  arguments <- many1 parseArgument
  return $ Call name arguments
  where
    parseArgument = do
      name <- many1 letter
      spaces
      return $ Identifier (ident name)

parseExpression =
  try parseInfix <|> parseDigit <|> try parseDeclaration <|> try parseCall <|>
  parseIdentifier

parseIdentifier = do
  name <- parseIdent
  return $ Identifier name

parseIdent = do
  name <- many1 letter
  return (ident name)

parseString :: ParsecT String u Identity Expression
parseString = do
  expr <- parseExpression
  spaces
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
    Call name args -> name ++ " " ++ unwords (printExpression <$> args)

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
          " " ++
          "(result i32)\n" ++
          indent ("(return \n" ++ indent (printWasmExpr expr) 2 ++ ")") 2 ++ ")"
        Infix op expr expr2 ->
          "(" ++
          opString op ++
          "\n" ++
          indent (printWasmExpr expr ++ "\n" ++ printWasmExpr expr2) 2 ++ ")\n"
        Identifier name -> "(get_local $" ++ name ++ ")"
        Call name args ->
          "(call $" ++
          name ++ "\n" ++ indent (unlines (printWasmExpr <$> args)) 2 ++ ")"
    paramsString args = unwords (paramString <$> args)
    paramString arg = "(param $" ++ arg ++ " i32)"
    opString op =
      case op of
        Add -> "i32.add"
        Subtract -> "i32.subtract"
        Multiply -> "i32.multiply"
        Divide -> "i32.divide"
