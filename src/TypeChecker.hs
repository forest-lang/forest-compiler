{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module TypeChecker
  ( checkModule
  , CompileError(..)
  , TypedModule(..)
  , TypedDeclaration(..)
  , TypedExpression(..)
  , TypedArgument(..)
  , typeOf
  , Type(..)
  , InvalidConstruct(..)
  , replaceGenerics
  ) where

import Data.Either
import qualified Data.Foldable as F
import Data.List (find, intercalate)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty, toList)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
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
  , typeConstructors :: Map TypeLambda [TypedConstructor]
  } deriving (Eq, Show)

data TypedConstructor =
  TypedConstructor Ident
                   [Type]
  deriving (Eq, Show)

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
  deriving (Eq, Show, Ord)

newtype TypeLambda =
  TypeLambda Ident
  deriving (Eq, Show, Ord)

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
         (NE.NonEmpty (TypedArgument, TypedExpression))
  | Let (NE.NonEmpty TypedDeclaration)
        TypedExpression
  | BetweenParens TypedExpression
  | String' Text
  deriving (Show, Eq, G.Generic)

data TypedArgument
  = TAIdentifier Type
                 Ident
  | TANumberLiteral Int
  | TADeconstruction Ident
                     [TypedArgument]
  deriving (Show, Eq, G.Generic)

addDeclarations :: CompileState -> [TypedDeclaration] -> CompileState
addDeclarations state declarations =
  state {typedDeclarations = declarations ++ typedDeclarations state}

addError :: CompileState -> CompileError -> CompileState
addError state error = state {errors = error : errors state}

addTypeLambda :: CompileState -> TypeLambda -> CompileState
addTypeLambda state tl = state {typeLambdas = tl : typeLambdas state}

addTypeConstructors ::
     CompileState -> TypeLambda -> [TypedConstructor] -> CompileState
addTypeConstructors state tl constructors =
  state
    { typeConstructors =
        Map.insertWith (++) tl constructors (typeConstructors state)
    }

checkModule :: Module -> Either (NonEmpty CompileError) TypedModule
checkModule (Module topLevels) =
  let initialState :: CompileState
      initialState =
        (CompileState
           { typeLambdas = []
           , errors = []
           , typedDeclarations = []
           , typeConstructors = Map.empty
           })
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
      addTypeConstructors
        (addDeclarations
           (addTypeLambda state tl)
           (makeDeclaration <$> NE.toList constructors))
        tl
        (makeTypeConstructor <$> NE.toList constructors)
      where tl = TypeLambda name
            returnType = Custom name (Generic <$> generics)
            makeDeclaration (Constructor name types) =
              TypedDeclaration
                name
                []
                (maybe returnType constructorType types)
                (TypeChecker.Number 0)
            makeTypeConstructor (Constructor name types) =
              TypedConstructor name (maybe [] constructorTypes types)
            constructorType t = foldr Lambda returnType (constructorTypes t)
            constructorTypes types =
              case types of
                CTConcrete i -> [stringToType i]
                CTParenthesized (CTApplied (CTConcrete a) (CTConcrete b)) ->
                  [Applied (TypeLambda a) (Generic b)]
                CTParenthesized t -> constructorTypes t
                CTApplied a b -> constructorTypes a ++ constructorTypes b
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
    (Custom name _, Custom name' _) -> name == name' -- TODO - now this is a hack
    _ -> a == b

checkDeclaration ::
     CompileState -> Declaration -> Either CompileError TypedDeclaration
checkDeclaration state declaration = do
  let (Declaration _ name args expr) = declaration
  annotationTypes <- inferDeclarationType (typeLambdas state) declaration
  let argsWithTypes = zip args (NE.toList annotationTypes)
  let locals = makeDeclaration <$> argsWithTypes
  expectedReturnType <-
    (case (NE.drop (length args) annotationTypes) of
       (x:xs) -> Right $ collapseTypes (x :| xs)
       _ -> Left $ CompileError (DeclarationError declaration) "Not enough args")
  let typedDeclaration =
        TypedDeclaration
          name
          argsWithTypes
          (foldr1 Lambda annotationTypes)
          (TypeChecker.Number 0)
  let actualReturnType =
        inferType (addDeclarations state (typedDeclaration : locals)) expr
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
            case (typeOf a, typeOf b) -- to anyone who sees this, I sincerely apologize
                  of
              (Lambda (Generic _) (Custom n x), b')
                | not . null $ x ->
                  Right $ TypeChecker.Apply (Applied (TypeLambda n) b') a b
              (Lambda (Generic n) r, b') ->
                Right (TypeChecker.Apply (replaceGenerics n b' r) a b)
              (Lambda (Applied tl (Generic n)) r, Applied tl' t)
                | tl == tl' ->
                  Right (TypeChecker.Apply (replaceGenerics n t r) a b)
              (Lambda (Lambda (Generic n) (Generic n')) r, Lambda a' b') ->
                Right
                  (TypeChecker.Apply
                     (replaceGenerics n' b' (replaceGenerics n a' r))
                     a
                     b)
              (Lambda x r, b') ->
                if x `typeEq` b'
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
      b <- sequence $ inferBranch v <$> branches
      allBranchesHaveSameType v b
      where inferBranch v (a, b) = do
              a' <- inferArgumentType state (typeOf v) a compileError
              let argDeclarations = declarationsFromTypedArgument a'
              b' <- inferType (addDeclarations state argDeclarations) b
              return (a', b')
            allBranchesHaveSameType ::
                 TypedExpression
              -> NonEmpty (TypedArgument, TypedExpression)
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

inferArgumentType ::
     CompileState
  -> Type
  -> Argument
  -> (Text -> CompileError)
  -> Either CompileError TypedArgument
inferArgumentType state valueType arg err =
  case arg of
    AIdentifier i -> Right $ TAIdentifier valueType i
    ANumberLiteral i ->
      if valueType == Num
        then Right $ TANumberLiteral i
        else Left $
             err $
             "case branch is type Int when value is type " <>
             printType valueType
    ADeconstruction name args ->
      let typeLambdaName =
            case valueType of
              Applied (TypeLambda i) _ -> Just i
              Custom i _ -> Just i
              _ -> Nothing
          typeLambda =
            typeLambdaName >>=
            (\tlName ->
               find (\(TypeLambda name') -> tlName == name') (typeLambdas state))
          constructorsForValue =
            typeLambda >>= (flip Map.lookup) (typeConstructors state)
          matchingConstructor =
            find (m name) (fromMaybe [] constructorsForValue)
          m name (TypedConstructor name' _) = name == name'
          deconstructionFields fields =
            sequence $
            (\(a, t) -> inferArgumentType state t a err) <$> zip args fields
       in case matchingConstructor of
            Just (TypedConstructor name fields) ->
              if length args == length fields
                then TADeconstruction name <$> deconstructionFields fields
                else Left $
                     err $
                     "Expected " <> s name <> " to have " <>
                     showT (length fields) <>
                     " fields, instead found " <>
                     showT (length args)
            Nothing ->
              Left $
              err $
              "no constructor named \"" <> s name <> "\" for " <>
              printType valueType <>
              " in scope."

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

declarationsFromTypedArgument :: TypedArgument -> [TypedDeclaration]
declarationsFromTypedArgument ta =
  case ta of
    TAIdentifier t n -> [TypedDeclaration n [] t (TypeChecker.Number 0)]
    TANumberLiteral _ -> []
    TADeconstruction _ args -> concatMap declarationsFromTypedArgument args

stringToType :: Ident -> Type
stringToType ident =
  case idToString ident of
    "String" -> Str
    "Int" -> Num
    i
      | i == T.toLower i -> Generic ident
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

mapType :: (Type -> Type) -> Type -> Type
mapType f t =
  case t of
    Num -> f t
    Str -> f t
    Lambda a b -> f (Lambda (mapType f a) (mapType f b))
    Applied tl t -> f (Applied tl (mapType f t))
    Generic _ -> f t
    Custom name types -> Custom name (mapType f <$> types)

replaceGenerics :: Ident -> Type -> Type -> Type
replaceGenerics name newType =
  mapType
    (\case
       Generic n
         | n == name -> newType
       other -> other)
