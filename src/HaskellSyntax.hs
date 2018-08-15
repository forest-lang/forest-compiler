{-# LANGUAGE OverloadedStrings #-}

module HaskellSyntax
  ( printExpression
  , printModule
  , parseModule
  , ParseError'
  , rws
  , s
  , expr
  , annotation
  , dataType
  , operatorToString
  , printDeclaration
  , indent2
  ) where

import Language

import Control.Monad (void)
import Data.Functor.Identity ()
import qualified Data.List.NonEmpty as NE
import Data.Semigroup
import Data.Text (Text, intercalate)
import qualified Data.Text as T
import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr

showT :: Show a => a -> Text
showT = T.pack . show

type Parser = Parsec Void Text

type ParseError' = ParseError Char Void

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space space1 lineComment Text.Megaparsec.empty <?> "whitespace"

sc :: Parser ()
sc =
  L.space (void $ takeWhile1P Nothing f) lineComment Text.Megaparsec.empty <?>
  "whitespace"
  where
    f x = x == ' ' || x == '\t'

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

expr :: Parser Expression
expr = makeExprParser (lexeme term) table <?> "expression"

term :: Parser Expression
term = L.lineFold scn $ \sc' -> terms >>= pApply sc'
  where
    terms = choice [pCase, pLet, identifier, parens, number, pString] <?> "term"
    pApply sc' e =
      (foldl1 Apply . (:) e <$> (some (try (sc' *> terms)) <* sc)) <|> return e

pString :: Parser Expression
pString =
  String' . T.pack <$> between (string "\"") (string "\"") (many $ notChar '"')

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser Expression
parens = BetweenParens <$> parens' expr

parens' :: Parser a -> Parser a
parens' = between (symbol "(" *> scn) (scn <* symbol ")")

table :: [[Operator Parser Expression]]
table =
  [ [InfixL (Infix Divide <$ char '/')]
  , [InfixL (Infix Multiply <$ char '*')]
  , [InfixL (Infix StringAdd <$ symbol "++")]
  , [InfixL (Infix Add <$ char '+')]
  , [InfixL (Infix Subtract <$ char '-')]
  ]

number :: Parser Expression
number = Number <$> (sc *> L.decimal)

rws :: [Text] -- list of reserved words
rws = ["case", "of", "let"]

makeIdent :: Parser Char -> Parser Char -> Parser Ident
makeIdent firstLetter rest = T.pack <$> p >>= check -- TODO - can we make p return Text?
  where
    p = (:) <$> firstLetter <*> many rest
    check x =
      if x `elem` rws
        then fail $ "keyword " <> show x <> " cannot be an identifier"
        else case x of
               "" -> fail "identifier must be longer than zero characters"
               _ -> return $ Ident $ NonEmptyString (T.head x) (T.tail x)

pIdent :: Parser Ident
pIdent = makeIdent letterChar alphaNumChar

pCapitalizedIdent :: Parser Ident
pCapitalizedIdent = makeIdent upperChar alphaNumChar

pLowerCaseIdent :: Parser Ident
pLowerCaseIdent = makeIdent lowerChar alphaNumChar

pCase :: Parser Expression
pCase = L.indentBlock scn p
  where
    p = do
      _ <- symbol "case"
      sc
      caseExpr <- expr
      scn
      _ <- symbol "of"
      return $
        L.IndentSome Nothing (return . Case caseExpr . NE.fromList) caseBranch
    caseBranch = do
      pattern' <- caseArgument
      sc
      _ <- symbol "->"
      scn
      branchExpr <- expr
      return (pattern', branchExpr)
    caseArgument = sc *> (deconstruction <|> identifier <|> numberLiteral)
    deconstruction = ADeconstruction <$> pCapitalizedIdent <*> many (try caseArgument)
    identifier = AIdentifier <$> pLowerCaseIdent
    numberLiteral = ANumberLiteral <$> L.decimal

pLet :: Parser Expression
pLet = do
  declarations <- pDeclarations
  _ <- symbol "in"
  scn
  expression <- expr
  return $ Let declarations expression
  where
    pDeclarations = L.indentBlock scn p
    p = do
      _ <- symbol "let"
      return $ L.IndentSome Nothing (return . NE.fromList) declaration

identifier :: Parser Expression
identifier = Identifier <$> pIdent

tld :: Parser TopLevel
tld = L.nonIndented scn (dataType <|> function)

dataType :: Parser TopLevel
dataType = do
  _ <- symbol "data"
  _ <- sc
  name <- pIdent
  generics <- many (sc *> pIdent)
  scn
  _ <- symbol "="
  constructor <- pConstructor
  constructors <- many (try (scn *> symbol "|" *> sc *> pConstructor))
  scn
  return $
    DataType $ ADT name generics (NE.fromList (constructor : constructors))
  where
    pConstructor = do
      name <- sc *> pIdent
      types <- maybeParse pConstructorType
      return $ Constructor name types
    pConstructorType =
      ((CTParenthesized <$> try (sc *> parens' pConstructorType)) <|>
       (CTConcrete <$> (sc *> pIdent))) >>=
      (\x -> CTApplied x <$> pConstructorType <|> return x)

function :: Parser TopLevel
function = Function <$> declaration

declaration :: Parser Declaration
declaration = do
  annotation' <- maybeParse annotation
  sc
  name <- pIdent
  args <- many (try (sc *> pIdent))
  sc
  _ <- symbol "="
  scn
  expression <- expr
  scn
  return $ Declaration annotation' name args expression

annotation :: Parser Annotation
annotation = do
  name <- pIdent
  sc
  _ <- symbol "::"
  sc
  types <- annotationTypes
  return $ Annotation name types

annotationTypes :: Parser (NE.NonEmpty AnnotationType)
annotationTypes = do
  firstType <- pType
  types <- many (sc *> symbol "->" *> pType)
  scn
  return (NE.fromList $ firstType : types)

pType :: Parser AnnotationType
pType =
  let typeInParens = parens' (Parenthesized <$> annotationTypes)
      concreteType = sc *> (Concrete <$> pIdent)
      typeApplication t = (TypeApplication t <$> try (sc *> pType)) <|> return t
   in typeInParens <|> concreteType >>= typeApplication

maybeParse :: Parser a -> Parser (Maybe a)
maybeParse parser = (Just <$> try parser) <|> Nothing <$ symbol "" -- TODO fix symbol "" hack

parseModule :: Text -> Either ParseError' Module
parseModule = parse pModule ""
  where
    pModule = Module <$> many tld <* eof

printModule :: Module -> Text
printModule (Module topLevel) = intercalate "\n\n" $ printTopLevel <$> topLevel

printTopLevel :: TopLevel -> Text
printTopLevel topLevel =
  case topLevel of
    Function declaration' -> printDeclaration declaration'
    DataType dataType' -> printDataType dataType'

printDataType :: ADT -> Text
printDataType (ADT name generics constructors) =
  "data " <> T.unwords (s <$> name : generics) <> "\n" <>
  indent2
    ("= " <>
     (intercalate "\n| " . NE.toList) (printConstructor <$> constructors))
  where
    printConstructor (Constructor name' types) =
      s name' <> " " <> maybe "" printType types
    printType t =
      case t of
        CTConcrete i -> s i
        CTApplied a b -> printType a <> " " <> printType b
        CTParenthesized t -> "(" <> printType t <> ")"

printDeclaration :: Declaration -> Text
printDeclaration (Declaration annotation' name args expr') =
  annotationAsString <> T.unwords ([s name] <> (s <$> args) <> ["="]) <> "\n" <>
  indent2 (printExpression expr')
  where
    annotationAsString = maybe "" printAnnotation annotation'

printAnnotation :: Annotation -> Text
printAnnotation (Annotation name types) =
  s name <> " :: " <> printTypes types <> "\n"
  where
    printTypes types' = intercalate " -> " (NE.toList (printType <$> types'))
    printType t =
      case t of
        Concrete i -> s i
        Parenthesized types' -> "(" <> printTypes types' <> ")"
        TypeApplication t' t'' -> printType t' <> " " <> printType t''

printExpression :: Expression -> Text
printExpression expression =
  case expression of
    Number n -> showT n
    Infix op expr' expr'' ->
      T.unwords
        [printExpression expr', operatorToString op, printSecondInfix expr'']
    Identifier name -> s name
    Apply expr' expr'' -> printExpression expr' <> " " <> printExpression expr''
    Case caseExpr patterns ->
      if isComplex caseExpr
        then "case\n" <> indent2 (printExpression caseExpr) <> "\nof\n" <>
             indent2 (printPatterns patterns)
        else "case " <> printExpression caseExpr <> " of\n" <>
             indent2 (printPatterns patterns)
    BetweenParens expr' ->
      if isComplex expr'
        then "(\n" <> indent2 (printExpression expr') <> "\n)"
        else "(" <> printExpression expr' <> ")"
    Let declarations expr' -> printLet declarations expr'
    String' str -> "\"" <> str <> "\""
  where
    printPatterns patterns = T.unlines $ NE.toList $ printPattern <$> patterns
    printPattern (argument, resultExpr) =
      printArgument argument <> " -> " <> printSecondInfix resultExpr
    printLet declarations expr' =
      intercalate "\n" $
      Prelude.concat
        [ ["let"]
        , indent2 . printDeclaration <$> NE.toList declarations
        , ["in"]
        , [indent2 $ printExpression expr']
        ]
    printArgument a =
      case a of
        AIdentifier i -> s i
        ADeconstruction ident args ->
          T.intercalate " " $ s ident : (printArgument <$> args)
        ANumberLiteral t -> showT t
    printSecondInfix expr' =
      if isComplex expr'
        then "\n" <> indent2 (printExpression expr')
        else printExpression expr'

isComplex :: Expression -> Bool
isComplex expr' =
  case expr' of
    Let {} -> True
    Case {} -> True
    Infix _ a b -> isComplex a || isComplex b
    _ -> False

indent :: Int -> Text -> Text
indent level str =
  intercalate "\n" $ (\line -> T.replicate level " " <> line) <$> T.lines str

indent2 :: Text -> Text
indent2 = indent 2

operatorToString :: OperatorExpr -> Text
operatorToString op =
  case op of
    Add -> "+"
    Subtract -> "-"
    Multiply -> "*"
    Divide -> "/"
    StringAdd -> "++"
