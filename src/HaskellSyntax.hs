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
  , pType
  , dataType
  , operatorToString
  , printDeclaration
  , indent2
  , printDataType
  ) where

import Language

import Control.Monad (void)
import Data.Functor.Identity ()
import Data.List.NonEmpty (NonEmpty(..))
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
    terms = choice [pCase, pLet, identifier, parens, try float, number, pString] <?> "term"
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

float :: Parser Expression
float = Float <$> do
  sc
  integer <- L.decimal
  symbol "."
  fractional <- L.decimal
  return $ fromIntegral integer + (fromIntegral fractional / 10) * signum (fromIntegral integer)

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
pCase = L.indentBlock scn p'
  where
    p' = indentArgs <$> (symbol "case" *> sc *> expr <* scn <* symbol "of")
    makeCase caseExpr = return . Case caseExpr . NE.fromList
    indentArgs caseExpr = L.IndentSome Nothing (makeCase caseExpr) caseBranch
    caseBranch = (,) <$> pArgument <*> (sc *> symbol "->" *> scn *> expr)

pArgument :: Parser Argument
pArgument = sc *> (deconstruction <|> identifier <|> numberLiteral)
  where
    deconstruction = ADeconstruction <$> pCapitalizedIdent <*> arguments
    arguments = many (try pArgument)
    identifier = AIdentifier <$> pLowerCaseIdent
    numberLiteral = ANumberLiteral <$> L.decimal

pLet :: Parser Expression
pLet = Let <$> pDeclarations <* symbol "in" <* scn <*> expr
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
dataType = DataType <$> (ADT <$> name <*> generics <*> (equals *> constructors))
  where
    name = symbol "data" *> sc *> pIdent
    equals = (scn <|> sc) *> symbol "="
    constructors = (:|) <$> pConstructor <*> otherConstructors <* scn
    otherConstructors = many (try (scn *> symbol "|" *> sc *> pConstructor))
    generics = many (sc *> pIdent)

pConstructor :: Parser Constructor
pConstructor = Constructor <$> (sc *> pIdent) <*> maybeParse pConstructorType
  where
    pConstructorType = (parens <|> concrete) >>= applied
    parens = CTParenthesized <$> try (sc *> parens' pConstructorType)
    concrete = CTConcrete <$> (sc *> pIdent)
    applied x = CTApplied x <$> pConstructorType <|> return x

function :: Parser TopLevel
function = Function <$> declaration

possiblyParenthesized :: Parser a -> Parser a
possiblyParenthesized p = parens' p <|> p

declaration :: Parser Declaration
declaration = Declaration <$> maybeAnnotation <*> name <*> args <*> expr'
  where
    maybeAnnotation = maybeParse annotation
    name = sc *> pIdent
    args = many (try (sc *> possiblyParenthesized pArgument))
    expr' = sc *> symbol "=" *> scn *> expr <* scn

annotation :: Parser Annotation
annotation = Annotation <$> pIdent <*> types
  where
    types = sc *> symbol "::" *> sc *> annotationTypes

annotationTypes :: Parser (NE.NonEmpty AnnotationType)
annotationTypes = (:|) <$> pType <*> many (sc *> symbol "->" *> pType) <* scn

pType :: Parser AnnotationType
pType = do
  let typeInParens = parens' (Parenthesized <$> annotationTypes)
      concreteType = Concrete <$> pIdent
  parts <- some (try (sc *> (typeInParens <|> concreteType)))
  return $
    case parts of
      [] -> error "well this can't rightly happen"
      [x] -> x
      xs -> foldl1 TypeApplication xs

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
  annotationAsString <>
  T.unwords ([s name] <> (printArgument Parens <$> args) <> ["="]) <>
  "\n" <>
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
    Float f -> showT f
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

printPatterns :: NonEmpty (Argument, Expression) -> Text
printPatterns patterns = T.unlines $ NE.toList $ printPattern <$> patterns

printPattern :: (Argument, Expression) -> Text
printPattern (argument, resultExpr) =
  printArgument NoParens argument <> " -> " <> printSecondInfix resultExpr

printLet :: NonEmpty Declaration -> Expression -> Text
printLet declarations expr' =
  intercalate "\n" $
  Prelude.concat
    [ ["let"]
    , indent2 . printDeclaration <$> NE.toList declarations
    , ["in"]
    , [indent2 $ printExpression expr']
    ]

data UseParensForDeconstruction
  = NoParens
  | Parens

printArgument :: UseParensForDeconstruction -> Argument -> Text
printArgument parens a =
  case (parens, a) of
    (_, AIdentifier i) -> s i
    (_, ANumberLiteral t) -> showT t
    (Parens, ADeconstruction ident args) ->
      "(" <> (T.intercalate " " $ s ident : (printArgument Parens <$> args)) <> ")"
    (NoParens, ADeconstruction ident args) ->
      T.intercalate " " $ s ident : (printArgument Parens <$> args)

printSecondInfix :: Expression -> Text
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
