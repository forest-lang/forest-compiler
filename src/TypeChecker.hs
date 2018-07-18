{-# LANGUAGE DeriveAnyClass #-}

module TypeChecker
  ( checkModule
  , CompileError(..)
  ) where

import Data.Either
import Data.List (find, intercalate)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty, toList)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (maybeToList)
import Debug.Trace (trace)
import Safe

import Language

newtype CompileError =
  CompileError String
  deriving (Eq, Show)

data CompileState = CompileState
  { errors :: [CompileError]
  , declarations :: [Declaration]
  } deriving (Eq, Show)

data Type
  = Num
  | Str
  | Lambda Type
           Type
  deriving (Eq, Show)

empty :: CompileState
empty = CompileState {declarations = [], errors = []}

isRight :: Either a b -> Bool
isRight either =
  case either of
    Left _ -> False
    Right _ -> True

addDeclaration :: CompileState -> Declaration -> CompileState
addDeclaration state declaration =
  CompileState
    {declarations = declaration : declarations state, errors = errors state}

addError :: CompileState -> CompileError -> CompileState
addError state error =
  CompileState
    {errors = error : errors state, declarations = declarations state}

checkModule :: Module -> Either (NonEmpty CompileError) ()
checkModule (Module topLevels) =
  let compileState :: CompileState
      compileState = foldl checkTopLevel empty topLevels
      e :: Maybe (NonEmpty CompileError)
      e = nonEmpty $ errors compileState
   in case e of
        Just e' -> Left e'
        Nothing -> Right ()

checkTopLevel :: CompileState -> TopLevel -> CompileState
checkTopLevel state topLevel =
  case topLevel of
    DataType _ -> state
    Function declaration ->
      let result = checkDeclaration (declarations state) declaration
       in case result of
            Right () -> addDeclaration state declaration
            Left e -> addError state e

checkDeclaration :: [Declaration] -> Declaration -> Either CompileError ()
checkDeclaration declarations declaration =
  let (Declaration annotation _ args expr) = declaration
      annotationTypes = inferDeclarationType declaration
      locals = makeDeclaration <$> zip args (NE.toList annotationTypes)
      expectedReturnType =
        typesToLambda (NE.fromList (NE.drop (length args) annotationTypes)) -- TODO remove NE.fromList
      actualReturnType = inferType (locals ++ declarations) expr
      makeDeclaration (name, t) =
        Declaration
          (Just (Annotation name (annotationType t :| [])))
          name
          []
          (Number 0)
      annotationType t =
        case t of
          Lambda a b -> Parenthesized (annotationType a :| [annotationType b])
          n -> Concrete $ (Ident $ NonEmptyString $ NE.fromList (printType n))
      syntacticallyCorrect =
        checkExpression (locals ++ declarations) expr annotation
      typeChecks =
        if actualReturnType == expectedReturnType
          then Right ()
          else Left
                 (CompileError $
                  "Expected " ++
                  show expectedReturnType ++ ", got " ++ show actualReturnType)
   in syntacticallyCorrect >> typeChecks

checkExpression ::
     [Declaration] -> Expression -> Maybe Annotation -> Either CompileError ()
checkExpression declarations expression _ =
  case expression of
    Number _ -> Right ()
    Apply expr' expr'' ->
      let fType = inferType declarations expr'
          aType = inferType declarations expr''
       in case fType of
            Lambda a _ ->
              if aType == a
                then Right ()
                else Left $
                     CompileError $
                     "Expected " ++ show a ++ ", got " ++ show aType
            _ ->
              Left $
              CompileError $
              "Tried to apply some bad shit " ++ show fType ++ " " ++ show aType
    Infix op a b ->
      if inferType declarations a == expected &&
         inferType declarations b == expected
        then Right ()
        else Left $ CompileError "a bad infix happened"
      where expected =
              case op of
                StringAdd -> Str
                _ -> Num
    String' _ -> Right ()
    x -> error $ "don't know how to check expression: " ++ show x

lambdaType :: Type -> Type -> [Type] -> Type
lambdaType left right remainder =
  case remainder of
    [] -> Lambda left right
    (x:xs) -> Lambda left (lambdaType right x xs)

inferType :: [Declaration] -> Expression -> Type
inferType declarations expr =
  case expr of
    Number _ -> Num
    String' _ -> Str
    BetweenParens expr -> inferType declarations expr
    Identifier name ->
      case find (m name) declarations of
        Just declaration -> typesToLambda . inferDeclarationType $ declaration
        Nothing ->
          error $ "not sure what identifier " ++ show ( name) ++ " refers to"
    Apply a b ->
      let aType = inferType declarations a
          bType = inferType declarations b
       in case aType of
            Lambda x r ->
              if x == bType
                then r
                else error $
                     "tried to apply the wrong type, expected " ++
                     show x ++ ", got " ++ show bType
            _ -> error "tried to apply something that isn't a function"
    Infix op a b ->
      if inferType declarations a == expected &&
         inferType declarations b == expected
        then expected
        else error "a bad infix happened"
      where expected =
              case op of
                StringAdd -> Str
                _ -> Num
    x -> error $ "cannot infer type of " ++ show x
  where
    m name (Declaration _ name' _ _) = name == name'

inferDeclarationType :: Declaration -> NE.NonEmpty Type
inferDeclarationType (Declaration annotation _ _ _) =
  case annotation of
    Just (Annotation _ types) -> annotationTypeToType <$> types
    Nothing -> error "currently need annotations"
  where
    annotationTypeToType t =
      case t of
        Concrete i -> stringToType i
        Parenthesized types -> reduceTypes types
    reduceTypes :: NE.NonEmpty AnnotationType -> Type
    reduceTypes types = foldr1 Lambda (annotationTypeToType <$> types)

typesToLambda :: NE.NonEmpty Type -> Type
typesToLambda = foldr1 Lambda

stringToType :: Ident -> Type
stringToType ident =
  case idToString ident of
    "String" -> Str
    "Int" -> Num
    s -> error $ "don't know about type " ++ show s

printType :: Type -> String
printType t =
  case t of
    Str -> "String"
    Num -> "Int"
    Lambda a r -> printType a ++ " -> " ++ printType r

printSignature :: [Type] -> String
printSignature types = intercalate " -> " (printType <$> types)
-- construct a graph of types, somehow
-- where the node are values and the edges are function application
--
-- add :: Int -> Int -> Int
-- add a b = a + b
--
-- node: a b (+) return
-- edges: + -> a        (+) is known to be of type Int -> Int -> Int, a is known to be of type Int
--        a -> b        Therefore (+) applied to a is Int -> Int
--        b -> return   
