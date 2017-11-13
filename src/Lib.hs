module Lib
  ( printWasm
  , printExpression
  , printModule
  , parseExpressionFromString
  , Expression(..)
  , Operator(..)
  ) where

import Data.Char
import Data.Functor.Identity
import Data.List
import Debug.Trace
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
  | Case Expression
         [(Expression, Expression)]
  | BetweenParens Expression
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
  a <- parseDigit <|> parseIdentifier
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
      argument <- parseExpression
      spaces
      return argument

parseBetweenParens = do
  char '('
  expr <- parseExpression
  char ')'
  return $ BetweenParens expr

parseExpression =
  parseBetweenParens <|> try parseCase <|> try parseInfix <|> parseDigit <|>
  try parseDeclaration <|>
  try parseCall <|>
  parseIdentifier

parseCase = do
  spaces
  string "case"
  spaces
  expr <- parseIdentifier
  spaces
  string "of"
  spaces
  patterns <- many1 parsePattern
  return $ Case expr patterns
  where
    parsePattern = do
      pattern' <- parseDigit <|> parseIdentifier
      spaces
      string "->"
      spaces
      expr <- parseExpression
      spaces
      return (pattern', expr)

parseIdentifier = do
  name <- parseIdent
  return $ Identifier name

parseIdent = do
  name <- many1 letter
  return (ident name)

parseString :: ParsecT String u Identity [Expression]
parseString = do
  expr <- many1 parseExpression
  spaces
  eof
  return expr

parseExpressionFromString :: String -> Either ParseError [Expression]
parseExpressionFromString = parse parseString ""

operatorToString :: Operator -> String
operatorToString op =
  case op of
    Add -> "+"
    Subtract -> "-"
    Multiply -> "*"
    Divide -> "/"

printModule :: [Expression] -> String
printModule expressions = intercalate "\n\n" $ map printExpression expressions

printExpression :: Expression -> String
printExpression expr =
  case expr of
    Number n -> show n
    Infix op expr expr2 ->
      unwords [printExpression expr, operatorToString op, printExpression expr2]
    Assignment name args expr ->
      name ++ " " ++ unwords args ++ " = \n" ++ indent (printExpression expr) 2
    Identifier name -> name
    Call name args -> name ++ " " ++ unwords (printExpression <$> args)
    Case caseExpr patterns ->
      "case " ++
      printExpression caseExpr ++ " of\n" ++ indent (printPatterns patterns) 2
    BetweenParens expr -> "(" ++ printExpression expr ++ ")"
  where
    printPatterns patterns = unlines $ map printPattern patterns
    printPattern (patternExpr, resultExpr) =
      printExpression patternExpr ++ " -> " ++ printExpression resultExpr

indent :: String -> Int -> String
indent str level =
  intercalate "\n" $ map (\line -> replicate level ' ' ++ line) (lines str)

printWasm :: [Expression] -> String
printWasm expr =
  "(module\n" ++ indent (intercalate "\n" $ map printWasmExpr expr) 2 ++ "\n)"
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
          indent ("(return \n" ++ indent (printWasmExpr expr) 2 ++ "\n)") 2 ++
          "\n)"
        Infix op expr expr2 ->
          "(" ++
          opString op ++
          "\n" ++
          indent (printWasmExpr expr ++ "\n" ++ printWasmExpr expr2) 2 ++ "\n)"
        Identifier name -> "(get_local $" ++ name ++ ")"
        Call name args ->
          "(call $" ++
          name ++ "\n" ++ indent (unlines (printWasmExpr <$> args)) 2 ++ "\n)"
        Case caseExpr patterns -> printCase caseExpr patterns
        BetweenParens expr -> printWasmExpr expr
      where
        printCase caseExpr patterns =
          "(if (result i32)\n" ++
          indent (printPatterns caseExpr patterns) 2 ++ "\n)"
        combinePatterns acc val = acc ++ "\n" ++ printPattern val
        printPattern (patternExpr, branchExpr) = printWasmExpr branchExpr
        firstCase patterns = fst (head patterns)
        printPatterns caseExpr patterns =
          intercalate "\n" $
          case length patterns of
            1 ->
              [ printComparator caseExpr (fst $ head patterns)
              , "(then \n" ++ indent (printPattern (head patterns)) 2 ++ "\n)"
              ]
            n ->
              [ printComparator caseExpr (fst $ head patterns)
              , "(then \n" ++ indent (printPattern (head patterns)) 2 ++ "\n)"
              , "(else \n" ++
                indent (printCase caseExpr (tail patterns)) 2 ++ "\n)"
              ]
        printComparator a b =
          intercalate
            "\n"
            [ "(i32.eq"
            , indent (printWasmExpr a) 2
            , indent (printWasmExpr b) 2
            , ")"
            ]
    paramsString args = unwords (paramString <$> args)
    paramString arg = "(param $" ++ arg ++ " i32)"
    opString op =
      case op of
        Add -> "i32.add"
        Subtract -> "i32.sub"
        Multiply -> "i32.mul"
        Divide -> "i32.div_s"
