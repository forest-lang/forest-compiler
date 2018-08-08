{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

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
import qualified Data.Foldable as F
import Data.List (find, intercalate)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty, toList)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe, maybeToList)
import Data.Semigroup
import Data.Sequence (replicateM)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace (trace)
import qualified Generics.Deriving as G
import Safe

import HaskellSyntax
import Language

showT :: Show a => a -> Text
showT = T.pack . show

data CompileError =
  CompileError InvalidConstruct
               Text
  deriving (Eq, Show)

data InvalidConstruct
  = DeclarationError Declaration
  | ExpressionError Expression
  deriving (Eq, Show)

data CompileState = CompileState
  { errors :: [CompileError]
  , typeLambdas :: [TypeLambda]
  , typedDeclarations :: [TypedDeclaration]
  } deriving (Eq, Show)

data Type
  = Num
  | Str
  | Lambda Type
           Type
  | Applied TypeLambda
            Type
  | Generic Ident
  | Custom Ident
           [Type]
  deriving (Eq, Show)

newtype TypeLambda =
  TypeLambda Ident
  deriving (Eq, Show)

newtype TypedModule =
  TypedModule [TypedDeclaration]

data TypedDeclaration =
  TypedDeclaration Ident
                   [(Ident, Type)]
                   Type
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
  | String' Text
  deriving (Show, Eq, G.Generic)

addDeclarations :: CompileState -> [TypedDeclaration] -> CompileState
addDeclarations state declarations =
  CompileState
    { typeLambdas = typeLambdas state
    , typedDeclarations = declarations ++ typedDeclarations state
    , errors = errors state
    }

addError :: CompileState -> CompileError -> CompileState
addError state error =
  CompileState
    { typeLambdas = typeLambdas state
    , typedDeclarations = typedDeclarations state
    , errors = error : errors state
    }

addTypeLambda :: CompileState -> TypeLambda -> CompileState
addTypeLambda state tl =
  CompileState
    { typeLambdas = tl : typeLambdas state
    , typedDeclarations = typedDeclarations state
    , errors = errors state
    }

checkModule :: Module -> Either (NonEmpty CompileError) TypedModule
checkModule (Module topLevels) =
  let initialState :: CompileState
      initialState =
        (CompileState {typeLambdas = [], errors = [], typedDeclarations = []})
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
    DataType (ADT name generics constructors) ->
      addDeclarations
        (addTypeLambda state tl)
        (makeDeclaration <$> NE.toList constructors)
      where tl = TypeLambda name
            makeDeclaration (Constructor name args) =
              TypedDeclaration
                name
                ((\x -> (x, Generic x)) <$> args)
                (constructorType args)
                (TypeChecker.Number 0)
            constructorType args =
              foldr
                Lambda
                (Custom name (Generic <$> generics))
                (stringToType <$> args)
    Function declaration ->
      let result = checkDeclaration state declaration
       in case result of
            Right t -> addDeclarations state [t]
            Left e -> addError state e

typeEq :: Type -> Type -> Bool
typeEq a b =
  case (a, b) of
    (Custom name (_:__), Applied (TypeLambda name') _) -> name == name'
    (Applied (TypeLambda name') _, Custom name (_:__)) -> name == name'
    _ -> a == b

checkDeclaration ::
     CompileState -> Declaration -> Either CompileError TypedDeclaration
checkDeclaration state declaration = do
  let (Declaration _ name args expr) = declaration
  annotationTypes <- inferDeclarationType (typeLambdas state) declaration
  let argsWithTypes = zip args (NE.toList annotationTypes)
  let locals = makeDeclaration <$> argsWithTypes
  let expectedReturnType =
        collapseTypes (NE.fromList (NE.drop (length args) annotationTypes)) -- TODO remove NE.fromList
  let actualReturnType = inferType (addDeclarations state locals) expr
  let typeChecks a =
        if typeOf a `typeEq` expectedReturnType
          then Right $
               TypedDeclaration
                 name
                 argsWithTypes
                 (foldr1 Lambda annotationTypes)
                 a
          else Left $
               CompileError
                 (DeclarationError declaration)
                 ("Expected " <> s name <> " to return type " <>
                  printType expectedReturnType <>
                  ", but instead got type " <>
                  printType (typeOf a))
  actualReturnType >>= typeChecks
  where
    makeDeclaration (i, t) =
      TypedDeclaration i [] t (TypeChecker.Identifier t i)

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

inferType :: CompileState -> Expression -> Either CompileError TypedExpression
inferType state expr =
  case expr of
    Language.Number n -> Right $ TypeChecker.Number n
    Language.String' s -> Right $ TypeChecker.String' s
    Language.BetweenParens expr -> inferType state expr
    Language.Identifier name ->
      case find (m name) declarations of
        Just (TypedDeclaration _ _ t _) -> Right $ TypeChecker.Identifier t name
        Nothing ->
          Left $
          compileError
            ("It's not clear what \"" <> idToString name <> "\" refers to")
    Language.Apply a b ->
      let typedExprs = (,) <$> inferType state a <*> inferType state b
          inferApplication (a, b) =
            case (typeOf a, typeOf b) of
              (Lambda (Generic _) (Custom n x), b') | not . null $ x ->
                Right $ TypeChecker.Apply (Applied (TypeLambda n) b') a b
              (Lambda x r, b') ->
                if x == b'
                  then Right (TypeChecker.Apply r a b)
                  else Left $
                       compileError
                         ("Function expected argument of type " <> printType x <>
                          ", but instead got argument of type " <>
                          printType b')
              _ ->
                Left $
                compileError $
                "Tried to apply a value of type " <> printType (typeOf a) <>
                " to a value of type " <>
                printType (typeOf b)
       in typedExprs >>= inferApplication
    Language.Infix op a b ->
      let expected =
            case op of
              StringAdd -> Str
              _ -> Num
          types = (,) <$> inferType state a <*> inferType state b
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
      v <- inferType state value
      b <- sequence $ inferBranch <$> branches
      allBranchesHaveSameType v b
      where inferBranch (a, b) = do
              a' <- inferType state a
              b' <- inferType state b
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
                types' ->
                  if all
                       (\case
                          (x:y:_) -> x `typeEq` y
                          _ -> False)
                       (F.toList <$>
                        replicateM 2 (typeOf . snd . NE.head <$> types'))
                    then Right
                           (TypeChecker.Case
                              (typeOf . snd $ NE.head (head types'))
                              value
                              types)
                    else Left $
                         compileError
                           ("Case expression has multiple return types: " <>
                            T.intercalate
                              ", "
                              (printType <$> NE.toList (typeOf . snd <$> types)))
    Language.Let declarations' value ->
      let branchTypes ::
               [TypedDeclaration]
            -> [Declaration]
            -> Either CompileError [TypedDeclaration]
          branchTypes typed untyped =
            case untyped of
              [] -> Right []
              (x:xs) ->
                checkDeclaration (addDeclarations state typed) x >>= \t ->
                  (:) t <$> branchTypes (typed ++ [t]) xs
       in branchTypes [] (NE.toList declarations') >>= \b ->
            TypeChecker.Let (NE.fromList b) <$>
            inferType (addDeclarations state b) value
  where
    m name (TypedDeclaration name' _ _ _) = name == name'
    compileError = CompileError $ ExpressionError expr
    declarations = typedDeclarations state

inferDeclarationType ::
     [TypeLambda] -> Declaration -> Either CompileError (NE.NonEmpty Type)
inferDeclarationType typeLambdas declaration =
  case annotation of
    Just (Annotation _ types) -> sequence $ annotationTypeToType <$> types
    Nothing -> Left $ compileError "For now, annotations are required."
  where
    (Declaration annotation _ _ _) = declaration
    compileError = CompileError $ DeclarationError declaration
    annotationTypeToType t =
      case t of
        Concrete i -> Right $ stringToType i
        Parenthesized types -> reduceTypes types
        TypeApplication a b ->
          case a of
            Concrete i ->
              case find (m i) typeLambdas of
                Just tl -> Applied tl <$> annotationTypeToType b
                Nothing ->
                  Left $
                  compileError $ "Could not find type lambda: " <> idToString i
              where m name (TypeLambda name') = name == name'
            _ -> error "nah"
    reduceTypes :: NE.NonEmpty AnnotationType -> Either CompileError Type
    reduceTypes types =
      collapseTypes <$> sequence (annotationTypeToType <$> types)

collapseTypes :: NE.NonEmpty Type -> Type
collapseTypes = foldr1 Lambda

stringToType :: Ident -> Type
stringToType ident =
  case idToString ident of
    "String" -> Str
    "Int" -> Num
    i | i == T.toLower i -> Generic ident
    _ -> Custom ident []

printType :: Type -> Text
printType t =
  case t of
    Str -> "String"
    Num -> "Int"
    Lambda a r -> printType a <> " -> " <> printType r
    Applied (TypeLambda name) b -> idToString name <> " " <> printType b
    Generic n -> idToString n
    Custom n ts -> T.intercalate " " $ idToString n : (printType <$> ts)

printSignature :: [Type] -> Text
printSignature types = T.intercalate " -> " (printType <$> types)
