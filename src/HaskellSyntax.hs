
{-# LANGUAGE OverloadedStrings #-}

module HaskellSyntax
  ( printExpression
  , printModule
  , parseModule
  , parseModuleWithLineInformation
  , ParseError'
  , Parser
  , reservedWords
  , SourceRange
  , s
  , parseExpr
  , annotation
  , pType
  , dataType
  , operatorToString
  , printDeclaration
  , indent2
  , LineInformation(..)
  , printDataType
  ) where

import Language

import Control.Monad (void)
import Data.Functor.Identity ()
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Data.Map (Map)
import Data.Functor.Identity
import qualified Data.Map as Map
import Data.Semigroup
import Data.Text (Text, intercalate)
import qualified Data.Text as Text
import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Control.Monad.Combinators.Expr
showT :: Show a => a -> Text
showT = Text.pack . show

type Parser = StateT LineInformation (ParsecT Void Text Identity)

type ParseError' = ParseErrorBundle Text Void

type SourceRange = (SourcePos, SourcePos)

data LineInformation = LineInformation
  { expressions :: Map Expression SourceRange
  , topLevels :: Map TopLevel SourceRange
  } deriving (Show, Eq)

setTopLevelPosition :: TopLevel -> SourceRange -> LineInformation -> LineInformation
setTopLevelPosition tl pos (LineInformation expressions topLevels) =
  LineInformation expressions (Map.insert tl pos topLevels)

setExpressionPosition :: Expression -> SourceRange -> LineInformation -> LineInformation
setExpressionPosition expression pos (LineInformation expressions topLevels) =
  LineInformation (Map.insert expression pos expressions) topLevels

-- Parser combinators
lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme whiteSpace

symbol :: Text -> Parser Text
symbol = Lexer.symbol whiteSpace

parens' :: Parser a -> Parser a
parens' =
  between
    (symbol "(" *> whiteSpaceWithNewlines)
    (whiteSpaceWithNewlines <* symbol ")")

possiblyParenthesized :: Parser a -> Parser a
possiblyParenthesized parser = parens' parser <|> parser

maybeParse :: Parser a -> Parser (Maybe a)
maybeParse parser = (Just <$> try parser) <|> Nothing <$ symbol "" -- TODO fix symbol "" hack

-- Parsers
whiteSpace :: Parser ()
whiteSpace =
  Lexer.space
    (void $ takeWhile1P Nothing charIsWhiteSpace)
    lineComment
    Text.Megaparsec.empty <?>
  "whitespace"
  where
    charIsWhiteSpace char = char == ' ' || char == '\t'

whiteSpaceWithNewlines :: Parser ()
whiteSpaceWithNewlines =
  Lexer.space space1 lineComment Text.Megaparsec.empty <?> "whitespace"

lineComment :: Parser ()
lineComment = Lexer.skipLineComment "#"

parseTerm :: Parser Expression
parseTerm =
  Lexer.lineFold whiteSpaceWithNewlines $ \whiteSpace' ->
    terms >>= pApply whiteSpace'
  where
    terms =
      choice [pCase, pLet, identifier, parens, try float, number, parseString] <?>
      "term"
    pApply whiteSpace' expression =
      (foldl1 Apply . (:) expression <$>
       (some (try (whiteSpace' *> terms)) <* whiteSpace)) <|>
      return expression

parseExpr :: Parser Expression
parseExpr = do
  _ <- whiteSpace
  startPos <- getSourcePos
  expression <- makeExprParser (lexeme parseTerm) table <?> "expression"
  endPos <- getSourcePos
  modify (setExpressionPosition expression (startPos, endPos))
  return expression
  where
    table :: [[Operator Parser Expression]]
    table =
      [ [InfixL (Infix Divide <$ char '/')]
      , [InfixL (Infix Multiply <$ char '*')]
      , [InfixL (Infix StringAdd <$ symbol "++")]
      , [InfixL (Infix Add <$ char '+')]
      , [InfixL (Infix Subtract <$ char '-')]
      ]

parseString :: Parser Expression
parseString =
  String' . Text.pack <$>
  between (string "\"") (string "\"") (many $ anySingleBut '"')

parens :: Parser Expression
parens = BetweenParens <$> parens' parseExpr

float :: Parser Expression
float =
  Float <$> do
    whiteSpace
    integer <- Lexer.decimal
    symbol "."
    fractional <- Lexer.decimal
    return $
      fromIntegral integer +
      (fromIntegral fractional / 10) * signumNoZero (fromIntegral integer)

signumNoZero :: Num a => Eq a => a -> a
signumNoZero 0 = 1
signumNoZero n = signum n

number :: Parser Expression
number = Number <$> (whiteSpace *> Lexer.decimal)

reservedWords :: [Text] -- list of reserved words
reservedWords = ["case", "of", "let"]

makeIdent :: Parser Char -> Parser Char -> Parser Ident
makeIdent firstLetter rest = Text.pack <$> p >>= check -- TODO - can we make p return Text?
  where
    p = (:) <$> firstLetter <*> many rest
    check text =
      if text `elem` reservedWords
        then fail $ "keyword " <> show text <> " cannot be an identifier"
        else case text of
               "" -> fail "identifier must be longer than zero characters"
               _ ->
                 return $
                 Ident $ NonEmptyString (Text.head text) (Text.tail text)

pIdent :: Parser Ident
pIdent = makeIdent letterChar alphaNumChar

pCapitalizedIdent :: Parser Ident
pCapitalizedIdent = makeIdent upperChar alphaNumChar

pLowerCaseIdent :: Parser Ident
pLowerCaseIdent = makeIdent lowerChar alphaNumChar

pCase :: Parser Expression
pCase = Lexer.indentBlock whiteSpaceWithNewlines parseCaseStart
  where
    parseCaseStart =
      indentArgs <$>
      (symbol "case" *> whiteSpace *> parseExpr <* whiteSpaceWithNewlines <*
       symbol "of")
    makeCase caseExpr = return . Case caseExpr . NE.fromList
    indentArgs caseExpr =
      Lexer.IndentSome Nothing (makeCase caseExpr) caseBranch
    caseBranch =
      (,) <$> pArgument <*>
      (whiteSpace *> symbol "->" *> whiteSpaceWithNewlines *> parseExpr)

pArgument :: Parser Argument
pArgument =
  whiteSpace *>
  possiblyParenthesized (deconstruction <|> identifier <|> numberLiteral)
  where
    deconstruction = ADeconstruction <$> pCapitalizedIdent <*> arguments
    arguments = many (try pArgument)
    identifier = AIdentifier <$> pLowerCaseIdent
    numberLiteral = ANumberLiteral <$> Lexer.decimal

pLet :: Parser Expression
pLet =
  Let <$> pDeclarations <* symbol "in" <* whiteSpaceWithNewlines <*> parseExpr
  where
    pDeclarations =
      Lexer.indentBlock whiteSpaceWithNewlines parseLetDeclarations
    parseLetDeclarations = do
      _ <- symbol "let"
      return $ Lexer.IndentSome Nothing (return . NE.fromList) declaration

identifier :: Parser Expression
identifier = Identifier <$> pIdent

topLevelDeclaration :: Parser TopLevel
topLevelDeclaration = do
  _ <- whiteSpaceWithNewlines
  startPos <- getSourcePos
  topLevel <- Lexer.nonIndented whiteSpaceWithNewlines (dataType <|> function)
  endPos <- getSourcePos
  modify (setTopLevelPosition topLevel (startPos, endPos))
  return topLevel

dataType :: Parser TopLevel
dataType = DataType <$> (ADT <$> name <*> generics <*> (equals *> constructors))
  where
    name = symbol "data" *> whiteSpace *> pIdent
    equals = (whiteSpaceWithNewlines <|> whiteSpace) *> symbol "="
    constructors =
      (:|) <$> pConstructor <*> otherConstructors <* whiteSpaceWithNewlines
    otherConstructors =
      many
        (try
           (whiteSpaceWithNewlines *> symbol "|" *> whiteSpace *> pConstructor))
    generics = many (whiteSpace *> pIdent)

pConstructor :: Parser Constructor
pConstructor =
  Constructor <$> (whiteSpace *> pIdent) <*> maybeParse pConstructorType
  where
    pConstructorType = (parens <|> concrete) >>= applied
    parens = CTParenthesized <$> try (whiteSpace *> parens' pConstructorType)
    concrete = CTConcrete <$> (whiteSpace *> pIdent)
    applied constructorType =
      CTApplied constructorType <$> pConstructorType <|> return constructorType

function :: Parser TopLevel
function = Function <$> declaration

declaration :: Parser Declaration
declaration = Declaration <$> maybeAnnotation <*> name <*> args <*> expression
  where
    maybeAnnotation = maybeParse annotation
    name = whiteSpace *> pIdent
    args = many (try (whiteSpace *> possiblyParenthesized pArgument))
    expression =
      whiteSpace *> symbol "=" *> whiteSpaceWithNewlines *> parseExpr <*
      whiteSpaceWithNewlines

annotation :: Parser Annotation
annotation = Annotation <$> pIdent <*> types
  where
    types = whiteSpace *> symbol "::" *> whiteSpace *> annotationTypes

annotationTypes :: Parser (NE.NonEmpty AnnotationType)
annotationTypes =
  (:|) <$> pType <*> many (whiteSpace *> symbol "->" *> pType) <*
  whiteSpaceWithNewlines

pType :: Parser AnnotationType
pType = do
  let typeInParens = parens' (Parenthesized <$> annotationTypes)
      concreteType = Concrete <$> pIdent
  parts <- some (try (whiteSpace *> (typeInParens <|> concreteType)))
  return $
    case parts of
      [] -> error "well this can't rightly happen"
      [x] -> x
      xs -> foldl1 TypeApplication xs

parseModuleWithLineInformation :: Text -> Either ParseError' (Module, LineInformation)
parseModuleWithLineInformation = parse (runStateT pModule (LineInformation Map.empty Map.empty)) ""
  where
    pModule = Module <$> many topLevelDeclaration <* eof

parseModule :: Text -> Either ParseError' Module
parseModule = parse (evalStateT pModule (LineInformation Map.empty Map.empty)) ""
  where
    pModule = Module <$> many topLevelDeclaration <* eof

-- Printers
printModule :: Module -> Text
printModule (Module topLevel) = intercalate "\n\n" $ printTopLevel <$> topLevel

printTopLevel :: TopLevel -> Text
printTopLevel topLevel =
  case topLevel of
    Function declaration' -> printDeclaration declaration'
    DataType dataType' -> printDataType dataType'

printDataType :: ADT -> Text
printDataType (ADT name generics constructors) =
  "data " <> Text.unwords (s <$> name : generics) <> "\n" <>
  indent2
    ("= " <>
     (intercalate "\n| " . NE.toList) (printConstructor <$> constructors))
  where
    printConstructor (Constructor name' types) =
      s name' <> " " <> maybe "" printType types
    printType constructorType =
      case constructorType of
        CTConcrete i -> s i
        CTApplied a b -> printType a <> " " <> printType b
        CTParenthesized constructorType ->
          "(" <> printType constructorType <> ")"

printDeclaration :: Declaration -> Text
printDeclaration (Declaration annotation' name args expression) =
  annotationAsString <>
  Text.unwords ([s name] <> (printArgument Parens <$> args) <> ["="]) <>
  "\n" <>
  indent2 (printExpression expression)
  where
    annotationAsString = maybe "" printAnnotation annotation'

printAnnotation :: Annotation -> Text
printAnnotation (Annotation name types) =
  s name <> " :: " <> printTypes types <> "\n"
  where
    printTypes types' = intercalate " -> " (NE.toList (printType <$> types'))
    printType annotationType =
      case annotationType of
        Concrete identifier -> s identifier
        Parenthesized types' -> "(" <> printTypes types' <> ")"
        TypeApplication leftType rightType ->
          printType leftType <> " " <> printType rightType

printExpression :: Expression -> Text
printExpression expression =
  case expression of
    Number number -> showT number
    Float float -> showT float
    Infix operator leftExpression rightExpression ->
      Text.unwords
        [ printExpression leftExpression
        , operatorToString operator
        , printSecondInfix rightExpression
        ]
    Identifier name -> s name
    Apply leftExpression rightExpression ->
      printExpression leftExpression <> " " <> printExpression rightExpression
    Case caseExpr patterns ->
      if isComplex caseExpr
        then "case\n" <> indent2 (printExpression caseExpr) <> "\nof\n" <>
             indent2 (printPatterns patterns)
        else "case " <> printExpression caseExpr <> " of\n" <>
             indent2 (printPatterns patterns)
    BetweenParens expression ->
      if isComplex expression
        then "(\n" <> indent2 (printExpression expression) <> "\n)"
        else "(" <> printExpression expression <> ")"
    Let declarations expression -> printLet declarations expression
    String' string -> "\"" <> string <> "\""

printPatterns :: NonEmpty (Argument, Expression) -> Text
printPatterns patterns = Text.unlines $ NE.toList $ printPattern <$> patterns

printPattern :: (Argument, Expression) -> Text
printPattern (argument, resultExpression) =
  printArgument NoParens argument <> " -> " <> printSecondInfix resultExpression

printLet :: NonEmpty Declaration -> Expression -> Text
printLet declarations expression =
  intercalate "\n" $
  Prelude.concat
    [ ["let"]
    , indent2 . printDeclaration <$> NE.toList declarations
    , ["in"]
    , [indent2 $ printExpression expression]
    ]

data UseParensForDeconstruction
  = NoParens
  | Parens

printArgument :: UseParensForDeconstruction -> Argument -> Text
printArgument parens argument =
  case (parens, argument) of
    (_, AIdentifier i) -> s i
    (_, ANumberLiteral t) -> showT t
    (Parens, ADeconstruction ident args) ->
      "(" <> (Text.intercalate " " $ s ident : (printArgument Parens <$> args)) <>
      ")"
    (NoParens, ADeconstruction ident args) ->
      Text.intercalate " " $ s ident : (printArgument Parens <$> args)

printSecondInfix :: Expression -> Text
printSecondInfix expression =
  if isComplex expression
    then "\n" <> indent2 (printExpression expression)
    else printExpression expression

isComplex :: Expression -> Bool
isComplex expr' =
  case expr' of
    Let {} -> True
    Case {} -> True
    Infix _ leftExpression rightExpression ->
      isComplex leftExpression || isComplex rightExpression
    _ -> False

indent :: Int -> Text -> Text
indent level string =
  intercalate "\n" $
  (Text.replicate level " " <>) <$> Text.lines string

indent2 :: Text -> Text
indent2 = indent 2

operatorToString :: OperatorExpr -> Text
operatorToString operator =
  case operator of
    Add -> "+"
    Subtract -> "-"
    Multiply -> "*"
    Divide -> "/"
    StringAdd -> "++"
