{-# LANGUAGE OverloadedStrings #-}

module JavaScriptSyntax
  ( printModule
  ) where

import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (toList)
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as T
import Language

showT :: Show a => a -> Text
showT = T.pack . show

indent :: Int -> Text -> Text
indent level str =
  T.intercalate "\n" $
  map (\line -> T.replicate level " " <> line) (T.lines str)

indent2 :: Text -> Text
indent2 = indent 2

printModule :: Module -> Text
printModule (Module topLevels) =
  (T.intercalate "\n\n" $ map printTopLevel topLevels) <> "\n"

printTopLevel :: TopLevel -> Text
printTopLevel topLevel =
  case topLevel of
    Function declaration -> printDeclaration declaration
    DataType (ADT name generics ctors) ->
      "type " <> s name <> printedGenerics <> " =" <>
      indent 2 printedCtors
      where printedGenerics =
              case generics of
                 [] -> ""
                 _ -> "<" <> T.intercalate ", " (s <$> generics) <> ">"
            printedCtors = T.intercalate " | " (printCtor <$> (NE.toList ctors))
            printCtor (Constructor name maybeType) =
              s name <> " " <> maybe "" printConstructorType maybeType
            printConstructorType ctorType =
              case ctorType of
                CTConcrete i -> s i
                CTApplied a b ->
                  printConstructorType a <> " " <> printConstructorType b
                CTParenthesized ct -> parens (printConstructorType ct)

printDeclaration :: Declaration -> Text
printDeclaration (Declaration _ name args expression) =
  "function " <> s name <> printedArgs <> " {\n  return " <>
  printExpression expression <>
  "\n}"
  where
    printedArgs = parens $ T.intercalate ", " $ map printArgument args

printExpression :: Expression -> Text
printExpression expression =
  case expression of
    Number number -> showT number
    Float f -> showT f
    Identifier identifier -> s identifier
    Infix operator a b ->
      T.intercalate
        " "
        [printExpression a, printOperator operator, printExpression b]
    String' string -> showT string
    Apply a b -> printExpression a <> parens (printExpression b)
    BetweenParens expression -> parens $ printExpression expression
    Case expression branches ->
      "switch " <> parens (printExpression expression) <> " {\n" <>
      indent 4 (printBranches branches) <>
      "\n  }"
    Let declarations expr ->
      indent
        2
        (T.intercalate
           "\n"
           ((printDeclaration <$>
            (NE.toList declarations)) <> [printExpression expr]))
  where
    printBranches branches =
      T.intercalate "\n" $ toList $ fmap printBranch branches
    printBranch (condition, body) =
      "case " <> printArgument condition <> ":\n" <>
      indent2 (printExpression body)

printArgument :: Argument -> Text
printArgument a =
  case a of
    AIdentifier n -> s n
    ADeconstruction name args ->
      s name <> parens (T.intercalate ", " (printArgument <$> args))
    ANumberLiteral i -> showT i

printOperator :: OperatorExpr -> Text
printOperator operator =
  case operator of
    Add -> "+"
    Subtract -> "-"
    Divide -> "/"
    Multiply -> "*"
    StringAdd -> "++"

parens :: Text -> Text
parens s = "(" <> s <> ")"
