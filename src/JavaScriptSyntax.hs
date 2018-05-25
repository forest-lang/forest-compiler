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
printDeclaration (Declaration _ name _ expression) =
  "function " ++ s name ++ "() { return " ++ printExpression expression ++ " }"

printExpression :: Expression -> String
printExpression expression =
  case expression of
    Number number -> show number
    _ -> undefined
