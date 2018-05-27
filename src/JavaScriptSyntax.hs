module JavaScriptSyntax
  ( printModule
  ) where

import Data.List (intercalate)
import Data.List.NonEmpty (toList)
import Language

indent :: Int -> String -> String
indent level str =
  intercalate "\n" $ map (\line -> replicate level ' ' ++ line) (lines str)

indent2 :: String -> String
indent2 = indent 2

printModule :: Module -> String
printModule (Module topLevels) =
  (intercalate "\n\n" $ map printTopLevel topLevels) ++ "\n"

printTopLevel :: TopLevel -> String
printTopLevel topLevel =
  case topLevel of
    Function declaration -> printDeclaration declaration
    DataType _ -> undefined

printDeclaration :: Declaration -> String
printDeclaration (Declaration _ name args expression) =
  "function " ++
  s name ++
  printedArgs ++ " {\n  return " ++ printExpression expression ++ "\n}"
  where
    printedArgs = parens $ intercalate ", " $ map s args

printExpression :: Expression -> String
printExpression expression =
  case expression of
    Number number -> show number
    Identifier identifier -> s identifier
    Infix operator a b ->
      intercalate
        " "
        [printExpression a, printOperator operator, printExpression b]
    String' string -> show string
    Call name args ->
      s name ++ parens (intercalate ", " (map printExpression args))
    BetweenParens expression -> parens $ printExpression expression
    Case expression branches ->
      "switch " ++
      parens (printExpression expression) ++
      " {\n" ++ indent 4 (printBranches branches) ++ "\n  }"
    _ -> error $ "not implemented " ++ show expression
  where
    printBranches branches =
      intercalate "\n" $ toList $ fmap printBranch branches
    printBranch (condition, body) =
      "case " ++
      printExpression condition ++ ":\n" ++ indent2 (printExpression body)

printOperator :: OperatorExpr -> String
printOperator operator =
  case operator of
    Add -> "+"
    Subtract -> "-"
    Divide -> "/"
    Multiply -> "*"
    StringAdd -> "++"

parens :: String -> String
parens s = "(" ++ s ++ ")"
