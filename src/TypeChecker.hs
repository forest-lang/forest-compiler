
module TypeChecker (checkModule, CompileError(..)) where

import Language

newtype CompileError = CompileError String
    deriving (Eq, Show)

checkModule :: Module -> Either CompileError ()
checkModule (Module _) =
  Right ()

