{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
  ( printExpression
  , printModule
  , parseExpressionFromString
  , Expression(..)
  , Module(..)
  , ParseError'
  , OperatorExpr(..)
  , Declaration(..)
  , NonEmptyString(..)
  , Ident(..)
  , rws
  , s
  , expr
  ) where

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Functor.Identity ()
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import Data.Semigroup
import Data.Text ()
import Data.Void (Void)
import qualified Generics.Deriving as G

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr

type Parser = Parsec Void String

type ParseError' = ParseError Char Void

newtype NonEmptyString =
  NonEmptyString (NE.NonEmpty Char)
  deriving (Show, Eq)

idToString :: Ident -> String
idToString (Ident str) = neToString str

neToString :: NonEmptyString -> String
neToString (NonEmptyString s) = NE.toList s

s :: Ident -> String
s = idToString

data OperatorExpr
  = Add
  | Subtract
  | Divide
  | Multiply
  deriving (Show, Eq, G.Generic)

newtype Ident =
  Ident NonEmptyString
  deriving (Show, Eq)

data Expression
  = Identifier Ident
  | Number Int
  | Assignment Declaration
  | Infix OperatorExpr
          Expression
          Expression
  | Call Ident
         [Expression]
  | Case Expression
         [(Expression, Expression)]
  | BetweenParens Expression
  | Let (NE.NonEmpty Declaration)
        Expression
  deriving (Show, Eq, G.Generic)

data Declaration =
  Declaration Ident
              [Ident]
              Expression
  deriving (Show, Eq, G.Generic)

newtype Module =
  Module [Declaration]
  deriving (Show, Eq, G.Generic)

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
term =
  sc *>
  (try pLet <|> try pCase <|> try assignment <|> parens <|> call <|> number)

termWithoutCall :: Parser Expression
termWithoutCall =
  sc *>
  (try pLet <|> try pCase <|> try assignment <|> parens <|> identifier <|>
   number)

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
rws = ["case", "of", "let"]

pIdent :: Parser Ident
pIdent = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x =
      if x `elem` rws
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else case NE.nonEmpty x of
               Just x -> return $ (Ident . NonEmptyString) x
               Nothing -> fail "identifier must be longer than zero characters"

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

pLet :: Parser Expression
pLet = do
  declarations <- pDeclarations
  symbol "in"
  scn
  expression <- expr

  return $ Let declarations expression
  where
    pDeclarations = L.indentBlock scn p
    p = do
      symbol "let"
      return $ L.IndentSome Nothing (return . NE.fromList) declaration

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

assignment :: Parser Expression
assignment = Assignment <$> declaration

tld :: Parser Declaration
tld = L.nonIndented scn declaration

declaration :: Parser Declaration
declaration = do
  sc
  name <- pIdent
  args <- many (try (sc *> pIdent))
  sc
  symbol "="
  scn
  expression <- expr
  scn
  return $ Declaration name args expression

parseModule :: Parser Module
parseModule = Module <$> many tld <* eof

parseExpressionFromString :: String -> Either ParseError' Module
parseExpressionFromString = parse parseModule ""

printModule :: Module -> String
printModule (Module declarations) =
  intercalate "\n\n" $ map printDeclaration declarations

printDeclaration :: Declaration -> String
printDeclaration (Declaration name args expr) =
  unwords ([s name] <> (s <$> args) <> ["="]) ++
  "\n" ++ indent2 (printExpression expr)

printExpression :: Expression -> String
printExpression expr =
  case expr of
    Number n -> show n
    Infix op expr expr2 ->
      unwords [printExpression expr, operatorToString op, printExpression expr2]
    Assignment declaration -> printDeclaration declaration
    Identifier name -> s name
    Call name args -> s name ++ " " ++ unwords (printExpression <$> args)
    Case caseExpr patterns ->
      "case " ++
      printExpression caseExpr ++ " of\n" ++ indent2 (printPatterns patterns)
    BetweenParens expr -> "(" ++ printExpression expr ++ ")"
    Let declarations expr -> printLet declarations expr
  where
    printPatterns patterns = unlines $ map printPattern patterns
    printPattern (patternExpr, resultExpr) =
      printExpression patternExpr ++ " -> " ++ printExpression resultExpr
    printLet declarations expr =
      intercalate "\n" $
      concat
        [ ["let"]
        , indent2 . printDeclaration <$> NE.toList declarations
        , ["in"]
        , [indent2 $ printExpression expr]
        ]

indent :: Int -> String -> String
indent level str =
  intercalate "\n" $ map (\line -> replicate level ' ' ++ line) (lines str)

indent2 :: String -> String
indent2 = indent 2

operatorToString :: OperatorExpr -> String
operatorToString op =
  case op of
    Add -> "+"
    Subtract -> "-"
    Multiply -> "*"
    Divide -> "/"
