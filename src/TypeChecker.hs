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
  , printType
  , TypeLambda(..)
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
  , types :: Map Ident Type
  , typedDeclarations :: [TypedDeclaration]
  , typeConstructors :: Map TypeLambda [TypedConstructor]
  } deriving (Eq, Show)

data TypedConstructor =
  TypedConstructor Ident
                   Int
                   [Type]
  deriving (Eq, Show)

data Type
  = Num
  | Str
  | Lambda Type
           Type
  | Applied Type
            Type
  | Generic Ident
  | TL TypeLambda
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
  | ADTConstruction Int [(Ident, Type)]
  deriving (Show, Eq, G.Generic)

data TypedArgument
  = TAIdentifier Type
                 Ident
  | TANumberLiteral Int
  | TADeconstruction Ident
                     Int
                     [TypedArgument]
  deriving (Show, Eq, G.Generic)

addDeclarations :: CompileState -> [TypedDeclaration] -> CompileState
addDeclarations state declarations =
  state {typedDeclarations = declarations ++ typedDeclarations state}

addError :: CompileState -> CompileError -> CompileState
addError state error = state {errors = error : errors state}

addTypeLambda :: CompileState -> TypeLambda -> CompileState
addTypeLambda state (TypeLambda name) =
  state
    { typeLambdas = TypeLambda name : typeLambdas state
    , types = Map.insert name (TL (TypeLambda name)) (types state)
    }

addTypeConstructors ::
     CompileState -> TypeLambda -> [TypedConstructor] -> CompileState
addTypeConstructors state tl constructors =
  state
    { typeConstructors =
        Map.insertWith (++) tl constructors (typeConstructors state)
    }

defaultTypes :: Map Ident Type
defaultTypes = Map.fromList [(ne "Int", Num), (ne "String", Str)]

checkModule :: Module -> Either (NonEmpty CompileError) TypedModule
checkModule (Module topLevels) =
  let initialState :: CompileState
      initialState =
        (CompileState
           { typeLambdas = []
           , errors = []
           , typedDeclarations = []
           , typeConstructors = Map.empty
           , types = defaultTypes
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
           (makeDeclaration <$> (zip [0..] $ NE.toList constructors)))
        tl
        (makeTypeConstructor <$> (zip [0..] $ NE.toList constructors))
      where tl = TypeLambda name
            returnType = foldl Applied (TL tl) (Generic <$> generics)
            makeDeclaration (tag, (Constructor name types)) =
              TypedDeclaration
                name
                (maybe [] constructorTypesToArgList types)
                (maybe returnType constructorType types)
                (TypeChecker.ADTConstruction tag (maybe [] constructorTypesToArgList types))
            makeTypeConstructor (tag, (Constructor name types)) =
              TypedConstructor name tag (maybe [] constructorTypes types)
            constructorType t = foldr Lambda returnType (constructorTypes t)
            constructorTypes ts =
              case ts of
                CTConcrete i ->
                  case stringToType (types state) undefined i of
                    Right x -> [x]
                    Left _ -> []
                CTParenthesized (CTApplied (CTConcrete a) (CTConcrete b)) ->
                  [Applied (TL (TypeLambda a)) (Generic b)]
                CTParenthesized t -> constructorTypes t
                CTApplied a b -> constructorTypes a ++ constructorTypes b
    Function declaration ->
      let result = checkDeclaration state declaration
       in case result of
            Right t -> addDeclarations state [t]
            Left e -> addError state e

constructorTypesToArgList :: ConstructorType -> [(Ident, Type)]
constructorTypesToArgList ct =
  case ct of
    CTConcrete i -> [(i, Num)]
    _ -> []

newtype Constraints =
  Constraints (Map Ident Type)
  deriving (Eq, Show)

typeEq :: Type -> Type -> Bool
typeEq a b =
  case typeConstraints a b of
    Just _ -> True
    _ -> False

mergePossibleConstraints :: [Maybe Constraints] -> Maybe Constraints
mergePossibleConstraints mConstraints =
  case mConstraints of
    [] -> Just (Constraints Map.empty)
    (Nothing:_) -> Nothing
    (Just constraints:xs) ->
      mergeConstraints constraints <$> mergePossibleConstraints xs

mergeConstraints :: Constraints -> Constraints -> Constraints
mergeConstraints (Constraints a) (Constraints b) = Constraints (Map.union a b) -- TODO handle clashes

-- you can't treat type a like an int
-- but you can call a function that accepts type a with an int,
-- as long as a is replaced with int in the interpretation of the type of that function
--
-- the rules for application differ from return type checking
--
-- for application, if we have a lambda with a generic value, we should replace that generic with our concrete value on the right
-- for return type checking, we need to be able to understand that we cannot coerce an "a" to a "b"
-- but that we can coerce a "Nothing :: Maybe a" to "Just 5 :: Maybe Int"
--
-- this is possible because the type of Nothing is really forall a. :: Maybe a
-- typeConstraints is currently used for both but that's a bad idea, it's only really good at application
typeConstraints :: Type -> Type -> Maybe Constraints
typeConstraints a b =
  case (a, b) of
    (Generic a', _) -> Just (Constraints (Map.insert a' b Map.empty))
    (Applied (TL a') t', Applied (TL b') (Generic g)) ->
      if a' == b'
        then Just (Constraints (Map.insert g t' Map.empty))
        else Nothing
    (Applied a b, Applied a' b') ->
      mergePossibleConstraints [typeConstraints a a', typeConstraints b b']
    (Lambda a b, Lambda x y) ->
      mergePossibleConstraints [typeConstraints a x, typeConstraints b y]
    (a', b') ->
      if a' == b'
        then Just (Constraints Map.empty)
        else Nothing

checkDeclaration ::
     CompileState -> Declaration -> Either CompileError TypedDeclaration
checkDeclaration state declaration = do
  let (Declaration _ name args expr) = declaration
  annotationTypes <- inferDeclarationType state declaration
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
        if typeOf a `typeEq` expectedReturnType -- TODO use typeConstraints here
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
    TypeChecker.ADTConstruction _ _ -> Lambda Num Num -- TODO - make this real

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
              (Lambda x r, b') ->
                case typeConstraints x b' of
                  Just constraints ->
                    Right
                      (TypeChecker.Apply (replaceGenerics constraints r) a b)
                  Nothing ->
                    Left $
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
                types'
                    -- TODO - there is a bug where we consider Result a b to be equal to Result c d,
                    --        failing to recognize the importance of whether a and b have been bound in the signature
                 ->
                  if all
                       (\case
                          (x:y:_) -> x `typeEq` y || y `typeEq` x
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
      let typeLambdaName v =
            case v of
              TL (TypeLambda i) -> Just i
              Applied (TL (TypeLambda i)) _ -> Just i
              Applied a _ -> typeLambdaName a
              _ -> Nothing
          typeLambda =
            typeLambdaName valueType >>=
            (\tlName ->
               find (\(TypeLambda name') -> tlName == name') (typeLambdas state))
          constructorsForValue =
            typeLambda >>= flip Map.lookup (typeConstructors state)
          matchingConstructor =
            find (m name) (fromMaybe [] constructorsForValue)
          m name (TypedConstructor name' _ _) = name == name'
          deconstructionFields fields =
            sequence $
            (\(a, t) -> inferArgumentType state t a err) <$> zip args fields
       in case matchingConstructor of
            Just (TypedConstructor name tag fields) ->
              if length args == length fields
                then TADeconstruction name tag <$> deconstructionFields fields
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
              " in scope." <>
              showT (typeLambda)

inferDeclarationType ::
     CompileState -> Declaration -> Either CompileError (NE.NonEmpty Type)
inferDeclarationType state declaration =
  case annotation of
    Just (Annotation _ types) -> sequence $ annotationTypeToType <$> types
    Nothing -> Left $ compileError "For now, annotations are required."
  where
    (Declaration annotation _ _ _) = declaration
    compileError = CompileError $ DeclarationError declaration
    annotationTypeToType t =
      case t of
        Concrete i -> stringToType (types state) compileError i
        Parenthesized types -> reduceTypes types
        TypeApplication a b -> inferTypeApplication a b
      where
        m name (TypeLambda name') = name == name'
        inferTypeApplication ::
             AnnotationType -> AnnotationType -> Either CompileError Type
        inferTypeApplication a b =
          case a of
            Concrete i ->
              case find (m i) (typeLambdas state) of
                Just tl -> Applied (TL tl) <$> annotationTypeToType b
                Nothing ->
                  Left $
                  compileError $ "Could not find type lambda: " <> idToString i
            Parenthesized a' ->
              Applied <$> reduceTypes a' <*> annotationTypeToType b
            TypeApplication a' b' ->
              Applied <$> inferTypeApplication a' b' <*> annotationTypeToType b
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
    TADeconstruction _ _ args -> concatMap declarationsFromTypedArgument args

stringToType ::
     Map Ident Type
  -> (Text -> CompileError)
  -> Ident
  -> Either CompileError Type
stringToType types compileError ident =
  if T.toLower i == i
    then Right $ Generic ident
    else case Map.lookup ident types of
           Just t -> Right t
           Nothing ->
             Left $ compileError $ "Could not find type " <> s ident <> "."
  where
    i = s ident

printType :: Type -> Text
printType t =
  case t of
    Str -> "String"
    Num -> "Int"
    Lambda a r -> printType a <> " -> " <> printType r
    Applied a b -> printType a <> " " <> printType b
    Generic n -> idToString n
    TL (TypeLambda typeLambda) -> idToString typeLambda

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
    TL _ -> f t

replaceGenerics :: Constraints -> Type -> Type
replaceGenerics (Constraints constraints) t =
  Map.foldrWithKey replaceGeneric t constraints

replaceGeneric :: Ident -> Type -> Type -> Type
replaceGeneric name newType =
  mapType
    (\case
       Generic n
         | n == name -> newType
       other -> other)

ne :: Text -> Ident
ne s = Ident $ NonEmptyString (T.head s) (T.tail s)
