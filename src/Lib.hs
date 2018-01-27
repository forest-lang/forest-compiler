{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( printExpression
  , printModule
  , parseExpressionFromString
  , Expression(..)
  , Module(..)
  , OperatorExpr(..)
  , expr
  ) where

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Functor.Identity()
import Data.List (intercalate)
import Data.Text()
import Data.Semigroup
import Data.Void (Void)

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

term :: Parser Expression
term = sc *> (try pCase <|> try declaration <|> parens <|> call <|> number)

termWithoutCall :: Parser Expression
termWithoutCall =
  sc *> (try pCase <|> try declaration <|> parens <|> identifier <|> number)

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser Expression
parens = BetweenParens <$> between (symbol "(") (symbol ")") expr

table :: [[Operator Parser Expression]]
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

parseExpressionFromString :: String -> Either ParseError' Module
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
      unwords ([name] <> args <> ["="]) ++ "\n" ++ indent (printExpression expr) 2
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

operatorToString :: OperatorExpr -> String
operatorToString op =
  case op of
    Add -> "+"
    Subtract -> "-"
    Multiply -> "*"
    Divide -> "/"
