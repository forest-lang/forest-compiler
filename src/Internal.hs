module Internal
  ( check
  , Expression(..)
  , Literal(..)
  , CompileError(..)
  ) where

import Data.List (find, intercalate)

-- How to build a type checker (and also a type inference)
-- Currently, we have parsers of Text -> Module, and printers of Module -> Text
-- However, the Language Module contains no information about types, beyond optional annotations
-- So you can express programs that make no sense, like this one
--
-- main = 1 + "test"
--
-- this is valid Forest syntax, but ideally would result in a type error
-- let's do this as simple as possible, so that I don't give up
-- let's just type check expressions!
-- starting with 1 + test
-- except we'll actually model that as (add 1 "test")
data Expression
  = Literal Literal
  | Call String
         [Expression]

data Literal
  = String String
  | Number Integer

newtype CompileError =
  CompileError String
  deriving (Show, Eq)

data Function =
  Function String
           [Type]
           Type

data Type =
  Type String
  deriving (Show, Eq)

stdlibFunctions :: [Function]
stdlibFunctions =
  [Function "add" [(Type "Number"), (Type "Number")] (Type "Number")]

number :: Type
number = Type "Number"

string :: Type
string = Type "String"

typeOf :: Expression -> Type
typeOf expr =
  case expr of
    Literal lit ->
      case lit of
        Number _ -> number
        String _ -> string
    Call _ _ -> number -- blatantly incorrect but that is fine for now

typeName :: Type -> String
typeName (Type name) = name

check :: Expression -> Either CompileError ()
check expr =
  case expr of
    Literal _ -> Right ()
    Call name args ->
      let argTypes = typeOf <$> args
          appropriateFunction (Function name' argTypes' _) =
            name == name' && argTypes == argTypes'
          candidateFunction = find appropriateFunction stdlibFunctions
       in case candidateFunction of
            Just _ -> Right ()
            Nothing ->
              Left (CompileError $ "No function " ++ name ++ " for " ++ (intercalate " -> " $ typeName <$> argTypes))
      -- find the types of each value
      -- can we find an appropriate function to call?
      -- if so, great, otherwise it's an error
