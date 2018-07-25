{-# LANGUAGE DeriveGeneric #-}

module TypeChecker
  ( checkModule
  , CompileError(..)
  , TypedModule(..)
  , TypedDeclaration(..)
  , TypedExpression(..)
  , typeOf
  , Type(..)
  , InvalidConstruct(..)
  ) where

import Data.Either
import Data.List (find, intercalate)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty, toList)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe, maybeToList)
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (trace)
import qualified Generics.Deriving as G
import Safe

import HaskellSyntax
import Language

data CompileError =
  CompileError InvalidConstruct
               String
  deriving (Eq, Show)

data InvalidConstruct
  = DeclarationError Declaration
  | ExpressionError Expression
  deriving (Eq, Show)

data CompileState = CompileState
  { errors :: [CompileError]
  , declarations :: [Declaration]
  , typedDeclarations :: [TypedDeclaration]
  } deriving (Eq, Show)

data Type
  = Num
  | Str
  | Lambda Type
           Type
  deriving (Eq, Show)

newtype TypedModule =
  TypedModule [TypedDeclaration]

data TypedDeclaration =
  TypedDeclaration Ident
                   [(Ident, Type)]
                   TypedExpression
  deriving (Show, Eq, G.Generic)

data TypedExpression
  = Identifier Type
               Ident
  | Number Int
  | Infix Type
          OperatorExpr
          TypedExpression
          TypedExpression
  | Apply Type
          TypedExpression
          TypedExpression
  | Case Type
         TypedExpression
         (NE.NonEmpty (TypedExpression, TypedExpression))
  | Let (NE.NonEmpty TypedDeclaration)
        TypedExpression
  | BetweenParens TypedExpression
  | String' String
  deriving (Show, Eq, G.Generic)

addDeclaration ::
     CompileState -> Declaration -> TypedDeclaration -> CompileState
addDeclaration state declaration typed =
  CompileState
    { declarations = declaration : declarations state
    , typedDeclarations = typed : typedDeclarations state
    , errors = errors state
    }

addError :: CompileState -> CompileError -> CompileState
addError state error =
  CompileState
    { declarations = declarations state
    , typedDeclarations = typedDeclarations state
    , errors = error : errors state
    }

topLevelToDeclaration :: TopLevel -> Maybe Declaration
topLevelToDeclaration tl =
  case tl of
    Function d -> Just d
    DataType _ -> Nothing

checkModule :: Module -> Either (NonEmpty CompileError) TypedModule
checkModule (Module topLevels) =
  let declarations :: [Declaration]
      declarations = mapMaybe topLevelToDeclaration topLevels
      initialState :: CompileState
      initialState =
        (CompileState
           {errors = [], declarations = declarations, typedDeclarations = []})
      compileState :: CompileState
      compileState = foldl checkTopLevel initialState topLevels
      e :: Maybe (NonEmpty CompileError)
      e = nonEmpty $ errors compileState
   in case e of
        Just e' -> Left e'
        Nothing -> Right (TypedModule (typedDeclarations compileState))

checkTopLevel :: CompileState -> TopLevel -> CompileState
checkTopLevel state topLevel =
  case topLevel of
    DataType _ -> state
    Function declaration ->
      let result = checkDeclaration (declarations state) declaration
       in case result of
            Right t -> addDeclaration state declaration t
            Left e -> addError state e

checkDeclaration ::
     [Declaration] -> Declaration -> Either CompileError TypedDeclaration
checkDeclaration declarations declaration = do
  let (Declaration _ name args expr) = declaration
  annotationTypes <- inferDeclarationType declaration
  let argsWithTypes = zip args (NE.toList annotationTypes)
  let locals = makeDeclaration <$> argsWithTypes
  let expectedReturnType =
        collapseTypes (NE.fromList (NE.drop (length args) annotationTypes)) -- TODO remove NE.fromList
  let actualReturnType = inferType (locals ++ declarations) expr
  let typeChecks a =
        if typeOf a == expectedReturnType
          then Right $ TypedDeclaration name argsWithTypes a
          else Left $
               CompileError
                 (DeclarationError declaration)
                 ("Expected " ++ s name ++ " to return type " ++
                  printType expectedReturnType ++ ", but instead got type " ++ printType (typeOf a))
  actualReturnType >>= typeChecks
  where
    makeDeclaration (name, t) =
      Declaration
        (Just (Annotation name (annotationType t :| [])))
        name
        []
        (Language.Number 0)
    annotationType t =
      case t of
        Lambda a b -> Parenthesized (annotationType a :| [annotationType b])
        n -> Concrete (Ident $ NonEmptyString $ NE.fromList (printType n))

lambdaType :: Type -> Type -> [Type] -> Type
lambdaType left right remainder =
  case remainder of
    [] -> Lambda left right
    (x:xs) -> Lambda left (lambdaType right x xs)

typeOf :: TypedExpression -> Type
typeOf t =
  case t of
    TypeChecker.Identifier t _ -> t
    TypeChecker.Apply t _ _ -> t
    TypeChecker.Number _ -> Num
    TypeChecker.Infix t _ _ _ -> t
    TypeChecker.Case t _ _ -> t
    TypeChecker.Let _ te -> typeOf te
    TypeChecker.BetweenParens te -> typeOf te
    TypeChecker.String' _ -> Str

inferType :: [Declaration] -> Expression -> Either CompileError TypedExpression
inferType declarations expr =
  case expr of
    Language.Number n -> Right $ TypeChecker.Number n
    Language.String' s -> Right $ TypeChecker.String' s
    Language.BetweenParens expr -> inferType declarations expr
    Language.Identifier name ->
      let makeIdentifier types =
            TypeChecker.Identifier (collapseTypes types) name
       in case find (m name) declarations of
            Just declaration ->
              makeIdentifier <$> inferDeclarationType declaration
            Nothing ->
              Left $
              compileError
                ("It's not clear what \"" ++ idToString name ++ "\" refers to")
    Language.Apply a b ->
      let typedExprs =
            (,) <$> inferType declarations a <*> inferType declarations b
          inferApplication (a, b) =
            case (typeOf a, typeOf b) of
              (Lambda x r, b') ->
                if x == b'
                  then Right (TypeChecker.Apply r a b)
                  else Left $
                       compileError
                         ("Function expected argument of type " ++ printType x ++ ", but instead got argument of type " ++ printType b')
              _ ->
                Left $
                compileError $ "Tried to apply a value of type " <> printType (typeOf a) <> " to a value of type " <> printType (typeOf b)
       in typedExprs >>= inferApplication
    Language.Infix op a b ->
      let expected =
            case op of
              StringAdd -> Str
              _ -> Num
          types = (,) <$> inferType declarations a <*> inferType declarations b
          checkInfix (a, b) =
            if typeOf a == expected && typeOf b == expected
              then Right (TypeChecker.Infix expected op a b)
              else Left $
                   compileError
                     ("No function exists with type " <> printType (typeOf a) <>
                      " " <>
                      operatorToString op <>
                      " " <>
                      printType (typeOf b))
       in types >>= checkInfix
    Language.Case value branches -> do
      v <- inferType declarations value
      b <- sequence $ inferBranch <$> branches
      allBranchesHaveSameType v b
      where inferBranch (a, b) = do
              a' <- inferType declarations a
              b' <- inferType declarations b
              return (a', b')
            allBranchesHaveSameType ::
                 TypedExpression
              -> NonEmpty (TypedExpression, TypedExpression)
              -> Either CompileError TypedExpression
            allBranchesHaveSameType value types =
              case NE.groupWith (typeOf . snd) types of
                [x] ->
                  Right
                    (TypeChecker.Case (typeOf . snd $ NE.head x) value types)
                _ ->
                  Left $
                  compileError
                    ("Case expression has multiple return types: " ++
                     intercalate
                       ", "
                       (printType <$> NE.toList (typeOf . snd <$> types)))
    Language.Let declarations' value ->
      let ds = NE.toList declarations' ++ declarations
          branchTypes = sequence (checkDeclaration ds <$> declarations')
       in TypeChecker.Let <$> branchTypes <*> inferType ds value
  where
    m name (Declaration _ name' _ _) = name == name'
    compileError = CompileError $ ExpressionError expr

inferDeclarationType :: Declaration -> Either CompileError (NE.NonEmpty Type)
inferDeclarationType declaration =
  case annotation of
    Just (Annotation _ types) -> sequence $ annotationTypeToType <$> types
    Nothing -> Left $ compileError "For now, annotations are required."
  where
    (Declaration annotation _ _ _) = declaration
    compileError = CompileError $ DeclarationError declaration
    annotationTypeToType t =
      case t of
        Concrete i -> stringToType i compileError
        Parenthesized types -> reduceTypes types
        TypeApplication _ _ ->
          error "lack information to infer type application"
    reduceTypes :: NE.NonEmpty AnnotationType -> Either CompileError Type
    reduceTypes types =
      collapseTypes <$> sequence (annotationTypeToType <$> types)

collapseTypes :: NE.NonEmpty Type -> Type
collapseTypes = foldr1 Lambda

stringToType :: Ident -> (String -> CompileError) -> Either CompileError Type
stringToType ident err =
  case idToString ident of
    "String" -> Right Str
    "Int" -> Right Num
    s -> Left $ err ("don't know about type " ++ show s)

printType :: Type -> String
printType t =
  case t of
    Str -> "String"
    Num -> "Int"
    Lambda a r -> printType a ++ " -> " ++ printType r

printSignature :: [Type] -> String
printSignature types = intercalate " -> " (printType <$> types)
