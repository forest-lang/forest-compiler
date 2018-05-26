module JavaScriptSyntax
  ( printModule
  ) where

import Data.List (intercalate)
import Language

printModule :: Module -> String
printModule (Module topLevels) =
  intercalate "\n\n" $ map printTopLevel topLevels

printTopLevel :: TopLevel -> String
printTopLevel topLevel =
  case topLevel of
    Function declaration -> printDeclaration declaration
    DataType _ -> undefined

printDeclaration :: Declaration -> String
printDeclaration (Declaration _ name args expression) =
  "function " ++
  s name ++
  "(" ++ printedArgs ++ ") { return " ++ printExpression expression ++ " }"
  where
    printedArgs = intercalate ", " $ map s args

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
      s name ++ "(" ++ intercalate ", " (map printExpression args) ++ ")"
    _ -> error $ "not implemented " ++ show expression

printOperator :: OperatorExpr -> String
printOperator operator =
  case operator of
    Add -> "+"
    _ -> undefined
