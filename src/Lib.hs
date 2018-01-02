{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( printWasm
  , printExpression
  , printModule
  , parseExpressionFromString
  , Expression(..)
  , Module(..)
  , OperatorExpr(..)
  , expr
  ) where

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Functor.Identity
import Data.List (intercalate)
import Data.Text (Text)
import Data.Void (Void)
import Debug.Trace

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr

type Parser = Parsec Void String

type ParseError' = ParseError Char Void

data OperatorExpr
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
  | Infix OperatorExpr
          Expression
          Expression
  | Call Ident
         [Expression]
  | Case Expression
         [(Expression, Expression)]
  | BetweenParens Expression
  | Negative Expression
  deriving (Show, Eq)

newtype Module =
  Module [Expression]
  deriving (Show, Eq)

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space space1 lineComment empty

sc :: Parser ()
sc = L.space (void $ takeWhile1P Nothing f) lineComment empty
  where
    f x = x == ' ' || x == '\t'

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

exprWithoutCall :: Parser Expression
exprWithoutCall = makeExprParser (lexeme termWithoutCall) table <?> "expression"

expr :: Parser Expression
expr = makeExprParser (lexeme term) table <?> "expression"

term = sc *> (try pCase <|> try declaration <|> parens <|> call <|> number)

termWithoutCall =
  sc *> (try pCase <|> try declaration <|> parens <|> identifier <|> number)

symbol = L.symbol sc

parens = BetweenParens <$> between (symbol "(") (symbol ")") expr

table =
  [ [InfixL (Infix Divide <$ char '/')]
  , [InfixL (Infix Multiply <$ char '*')]
  , [InfixL (Infix Add <$ char '+')]
  , [InfixL (Infix Subtract <$ char '-')]
  ]

number :: Parser Expression
number = Number <$> (sc *> L.decimal)

rword :: String -> Parser ()
rword w = lexeme (string w *> notFollowedBy alphaNumChar)

rws :: [String] -- list of reserved words
rws = ["case", "of"]

pIdent :: Parser Ident
pIdent = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x =
      if x `elem` rws
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else return x

pCase :: Parser Expression
pCase = L.indentBlock scn p
  where
    p = do
      symbol "case"
      sc
      caseExpr <- expr
      sc
      symbol "of"
      return $ L.IndentSome Nothing (return . Case caseExpr) caseBranch
    caseBranch = do
      sc
      pattern' <- number <|> identifier
      sc
      symbol "->"
      branchExpr <- expr
      return (pattern', branchExpr)

call :: Parser Expression
call = do
  name <- pIdent
  args <- many (try exprWithoutCall)
  return $
    case length args of
      0 -> Identifier name
      _ -> Call name args

identifier :: Parser Expression
identifier = Identifier <$> pIdent

declaration :: Parser Expression
declaration = do
  sc
  name <- pIdent
  args <- many (try (sc *> pIdent))
  sc
  symbol "="
  scn
  expression <- expr
  scn
  return $ Assignment name args expression

topLevelDeclaration :: Parser Expression
topLevelDeclaration = L.nonIndented scn declaration

parseModule :: Parser Module
parseModule = Module <$> many topLevelDeclaration <* eof

parseExpressionFromString = parse parseModule ""

printModule :: Module -> String
printModule (Module expressions) =
  intercalate "\n\n" $ map printExpression expressions

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

printWasm :: Module -> String
printWasm (Module expressions) =
  "(module\n" ++
  indent (intercalate "\n" $ map printWasmExpr expressions) 2 ++ "\n)"
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
          indent ("(return\n" ++ indent (printWasmExpr expr) 2 ++ "\n)") 2 ++
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
              , "(then\n" ++ indent (printPattern (head patterns)) 2 ++ "\n)"
              , "(else (i32.const 0))"
              ]
            n ->
              [ printComparator caseExpr (fst $ head patterns)
              , "(then\n" ++ indent (printPattern (head patterns)) 2 ++ "\n)"
              , "(else\n" ++
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

operatorToString :: OperatorExpr -> String
operatorToString op =
  case op of
    Add -> "+"
    Subtract -> "-"
    Multiply -> "*"
    Divide -> "/"
