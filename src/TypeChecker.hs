{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module TypeChecker
  ( checkModule
  , checkModuleWithLineInformation
  , ConstructorSymbol(..)
  , CompileError(..)
  , Symbol(..)
  , TypedModule(..)
  , TypedDeclaration(..)
  , TypedExpression(..)
  , TypedArgument(..)
  , ClosureBinding(..)
  , Binding(..)
  , Scope(..)
  , typeOf
  , Type(..)
  , InvalidConstruct(..)
  , replaceGenerics
  , printType
  , TypeLambda(..)
  ) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Data.Either
import qualified Data.Foldable as F
import Data.Functor.Identity
import Data.List (find, intercalate)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty, toList)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Set.Ordered (OSet)
import qualified Data.Set.Ordered as OSet
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
               (Maybe SourceRange)
               Text
  deriving (Eq, Show)

data InvalidConstruct
  = DeclarationError Declaration
  | ExpressionError Expression
  | DataTypeError ADT
  deriving (Eq, Show)

data CompileState = CompileState
  { errors :: [CompileError]
  , typeLambdas :: [TypeLambda]
  , types :: Map Ident Type
  , typedDeclarations :: [(Scope, TypedDeclaration)]
  , typeConstructors :: Map TypeLambda [TypedConstructor]
  } deriving (Eq, Show)

data TypedConstructor =
  TypedConstructor Symbol
                   Int
                   [Type]
  deriving (Eq, Show)

data Type
  = Num
  | Float' -- TODO a better name, perhaps declare in another module? :(
  | Str
  | Lambda Type
           Type
  | Applied Type
            Type
  | Generic Ident
  | TL TypeLambda -- TODO - need a better name. is this just an ADT
  deriving (Eq, Show, Ord)

newtype TypeLambda =
  TypeLambda Ident
  deriving (Eq, Show, Ord)

newtype TypedModule =
  TypedModule [TypedDeclaration]
  deriving (Eq, Show)

data ClosureBinding =
  ClosureBinding Symbol
                 Type
  deriving (Show, Eq, G.Generic, Ord)

data TypedDeclaration =
  TypedDeclaration Symbol
                   [TypedArgument]
                   (OSet ClosureBinding)
                   Type
                   TypedExpression
  deriving (Show, Eq, G.Generic)

data Symbol =
  Symbol Int
         Ident
  deriving (Show, Eq, G.Generic, Ord)

data Scope
  = Local
  | Closure
  | Global
  deriving (Show, Eq, G.Generic, Ord)

data Binding =
  Binding Scope Symbol
  deriving (Show, Eq, G.Generic, Ord)

data TypedExpression
  = Identifier Type
               Binding
               (OSet ClosureBinding)
  | Number Int
  | Float Float
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
  | String' Text
  | ADTConstruction Int FunctionType
                    [TypedArgument]
  deriving (Show, Eq, G.Generic)

data TypedArgument
  = TAIdentifier Type
                 Symbol
  | TANumberLiteral Int -- TODO this shouldn't really be propagated, it's syntax sugar
  | TADeconstruction Symbol
                     ConstructorSymbol
                     Int
                     [TypedArgument]
  deriving (Show, Eq, G.Generic)

newtype ConstructorSymbol =
  ConstructorSymbol Symbol
  deriving (Show, Eq, G.Generic)

data Symbols =
  Symbols Int
          (Map Int Symbol)

type CompilationSymbolsT a = ExceptT CompileError (StateT Symbols a)

type CompilationSymbols = CompilationSymbolsT Identity

type ClosureBindingsT = State (OSet ClosureBinding)

type DeclarationCompilation a = CompilationSymbolsT ClosureBindingsT a

liftCompilationState :: CompilationSymbols a -> DeclarationCompilation a
liftCompilationState = mapExceptT (mapStateT transform)
  where
    transform ::
         Identity (Either CompileError a, Symbols)
      -> StateT (OSet ClosureBinding) Identity (Either CompileError a, Symbols)
    transform = return . runIdentity

runClosureBindings ::
     DeclarationCompilation a -> CompilationSymbols (a, OSet ClosureBinding)
runClosureBindings = mapExceptT (mapStateT f)
  where
    f :: StateT (OSet ClosureBinding) Identity (Either CompileError a, Symbols)
      -> Identity (Either CompileError (a, OSet ClosureBinding), Symbols)
    f s =
      let ((a, symbols), bindings) = runState s OSet.empty
       in return ((, bindings) <$> a, symbols)

addClosureBinding :: ClosureBinding -> DeclarationCompilation ()
addClosureBinding cb = lift $ lift $ modify (OSet.>| cb)

getSymbol :: Ident -> CompilationSymbols Symbol
getSymbol i = do
  (Symbols id symbolMap) <- lift $ get
  let newId = id + 1
  let symbol = Symbol newId i
  let mapWithInsert = Map.insert newId symbol symbolMap
  lift $ put (Symbols newId mapWithInsert)
  return $ symbol

expressionPosition :: LineInformation -> Expression -> Maybe SourceRange
expressionPosition (LineInformation expressionPositions _) expr =
  Map.lookup expr expressionPositions

topLevelPosition :: LineInformation -> TopLevel -> Maybe SourceRange
topLevelPosition (LineInformation _ topLevelPositions) topLevel =
  Map.lookup topLevel topLevelPositions

addDeclarations :: CompileState -> [TypedDeclaration] -> CompileState
addDeclarations state declarations =
  state
    { typedDeclarations =
        ((Global, ) <$> declarations) <> typedDeclarations state
    }

addLocals :: CompileState -> [TypedDeclaration] -> CompileState
addLocals state declarations =
  state
    { typedDeclarations =
        ((Local, ) <$> declarations) <> typedDeclarations state
    }

addError :: CompileState -> CompileError -> CompileState
addError state error = state {errors = error : errors state}

addErrors :: CompileState -> [CompileError] -> CompileState
addErrors state newErrors = state {errors = newErrors <> errors state}

addTypeLambda :: CompileState -> TypeLambda -> CompileState
addTypeLambda state (TypeLambda name) =
  state
    { typeLambdas = TypeLambda name : typeLambdas state
    , types = Map.insert name (TL (TypeLambda name)) (types state)
    }

addTypeConstructors ::
     CompileState -> TypeLambda -> [TypedConstructor] -> CompileState
addTypeConstructors state typeLambda constructors =
  state
    { typeConstructors =
        Map.insertWith (++) typeLambda constructors (typeConstructors state)
    }

markClosureBoundary :: CompileState -> CompileState
markClosureBoundary state =
  state {typedDeclarations = (Closure, ) <$> (snd <$> typedDeclarations state)}

defaultTypes :: Map Ident Type
defaultTypes =
  Map.fromList [(ne "Int", Num), (ne "String", Str), (ne "Float", Float')]

checkModuleWithLineInformation ::
     Module
  -> Maybe LineInformation
  -> Either (NonEmpty CompileError) TypedModule
checkModuleWithLineInformation (Module topLevels) possibleLineInformation =
  let initialState :: CompileState
      initialState =
        (CompileState
           { typeLambdas = []
           , errors = []
           , typedDeclarations = []
           , typeConstructors = Map.empty
           , types = defaultTypes
           })
      lineInformation =
        fromMaybe (LineInformation Map.empty Map.empty) possibleLineInformation
      compileState :: CompileState
      compileState =
        let resultWithError =
              foldM (checkTopLevel lineInformation) initialState topLevels
            result =
              evalState (runExceptT resultWithError) (Symbols 0 Map.empty)
         in case result of
              Right a -> a
              Left e -> error $ "Encountered a problem compiling: " <> show e
      possibleErrors :: Maybe (NonEmpty CompileError)
      possibleErrors = nonEmpty $ errors compileState
   in case possibleErrors of
        Just errors -> Left errors
        Nothing -> Right (TypedModule (snd <$> typedDeclarations compileState))

checkModule :: Module -> Either (NonEmpty CompileError) TypedModule
checkModule m = checkModuleWithLineInformation m Nothing

-- TODO - make this function a bit easier to read
checkDataType ::
     CompileState -> ADT -> Maybe SourceRange -> CompilationSymbols CompileState
checkDataType state adt@(ADT name generics constructors) position = do
  (typedCtors, typedDecls, errors) <- result
  case errors of
    [] ->
      let state' = addTypeLambda state typeLambda
          state'' = addDeclarations state' typedDecls
          state''' = addTypeConstructors state'' typeLambda typedCtors
       in return state'''
    _ -> return $ addErrors state errors
  where
    result = process (NE.toList constructors) 0 [] [] []
    process ::
         [Constructor]
      -> Int
      -> [TypedConstructor]
      -> [TypedDeclaration]
      -> [CompileError]
      -> CompilationSymbols ( [TypedConstructor]
                            , [TypedDeclaration]
                            , [CompileError])
    process ctors i typedCtors typedDecls errors =
      case ctors of
        x@(Constructor name _):xs ->
          let doWork = do
                symbol <- getSymbol name
                constructor <- makeTypeConstructor (i, x, symbol)
                declaration <- makeDeclaration (i, x, symbol)
                process
                  xs
                  (i + 1)
                  (typedCtors <> [constructor])
                  (typedDecls <> [declaration])
                  errors
              handler e =
                process xs (i + 1) typedCtors typedDecls (errors <> [e])
           in doWork `catchError` handler
        [] -> return (typedCtors, typedDecls, errors)
    typeLambda = TypeLambda name
    returnType = foldl Applied (TL typeLambda) (Generic <$> generics)
    makeDeclaration ::
         (Int, Constructor, Symbol) -> CompilationSymbols TypedDeclaration
    makeDeclaration (tag, (Constructor _ types'), symbol) =
      let charToArgument :: (Char, Type) -> CompilationSymbols TypedArgument
          charToArgument (char, argType) = do
            symbol <- getSymbol (ne $ T.singleton char)
            lift $ return $ TAIdentifier argType symbol
          argList :: CompilationSymbols [Type]
          argList = maybe (return []) constructorTypes types'
          argsWithTypes :: CompilationSymbols [(Char, Type)]
          argsWithTypes = (zip ['a' ..] <$> argList)
          arguments :: CompilationSymbols [TypedArgument]
          arguments = do
            a <- argsWithTypes
            sequence $ charToArgument <$> a
          declarationFromType :: Type -> [TypedArgument] -> TypedDeclaration
          declarationFromType t typedArgument =
            TypedDeclaration
              symbol
              typedArgument
              OSet.empty
              t
              (TypeChecker.ADTConstruction tag Standard typedArgument)
          rType :: CompilationSymbols Type
          rType = (maybe (return returnType) constructorType types')
       in declarationFromType <$> rType <*> arguments
    makeTypeConstructor ::
         (Int, Constructor, Symbol) -> CompilationSymbols TypedConstructor
    makeTypeConstructor (tag, (Constructor _ types), symbol) =
      TypedConstructor symbol tag <$> (maybe (return []) constructorTypes types)
    constructorType :: ConstructorType -> CompilationSymbols Type
    constructorType ct = foldr Lambda returnType <$> (constructorTypes ct)
    errorMessage = CompileError (DataTypeError adt) position
    constructorTypes :: ConstructorType -> CompilationSymbols [Type]
    constructorTypes ct =
      case ct of
        CTConcrete identifier ->
          (\x -> [x]) <$>
          findTypeFromIdent
            ((Map.insert name returnType) $ types state)
            errorMessage
            identifier
        CTParenthesized (CTApplied (CTConcrete a) (CTConcrete b)) ->
          return [Applied (TL (TypeLambda a)) (Generic b)]
        CTParenthesized ct -> constructorTypes ct
        CTApplied a b -> do
          (<>) <$> constructorTypes a <*> constructorTypes b

checkTopLevel ::
     LineInformation
  -> CompileState
  -> TopLevel
  -> CompilationSymbols CompileState
checkTopLevel lineInformation state topLevel =
  case topLevel of
    DataType adt -> checkDataType state adt position
    Function declaration -> (checkFunc declaration) `catchError` handler
  where
    checkFunc :: Declaration -> CompilationSymbols CompileState
    checkFunc declaration = do
      result <- checkDeclaration state declaration position exprPosition
      return $ addDeclarations state [result]
    handler e = return $ addError state e
    position = topLevelPosition lineInformation topLevel
    exprPosition = expressionPosition lineInformation

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
-- typeConstraints is currentypeLambday used for both but that's a bad idea, it's only really good at application
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
     CompileState
  -> Declaration
  -> Maybe SourceRange
  -> (Expression -> Maybe SourceRange)
  -> CompilationSymbols TypedDeclaration
checkDeclaration state declaration position exprPosition = do
  let (Declaration _ name args expr) = declaration
  symbol <- getSymbol name
  annotationTypes <- inferDeclarationType state declaration position
  -- TODO - is sequence right here?
  argsWithTypes <-
    sequence $
    uncurry (inferArgumentType state compileError) <$>
    zip (NE.toList annotationTypes) args
  let locals = concatMap makeDeclarations argsWithTypes
  expectedReturnType <-
    (case (NE.drop (length args) annotationTypes) of
       (x:xs) -> return $ collapseTypes (x :| xs)
       _ -> throwError $ compileError "Not enough args") -- TODO - could be too many?
  let typedDeclaration =
        TypedDeclaration
          symbol
          argsWithTypes
          OSet.empty
          (foldr1 Lambda annotationTypes)
          (TypeChecker.Number 0)
  let actualReturnType =
        inferType
          (addLocals state (typedDeclaration : locals))
          expr
          exprPosition
  let typeChecks ::
           (TypedExpression, OSet ClosureBinding)
        -> CompilationSymbols TypedDeclaration
      typeChecks (typedExpression, closureBindings) =
        if typeOf typedExpression `typeEq` expectedReturnType -- TODO use typeConstraints here
          then return $
               TypedDeclaration
                 symbol
                 argsWithTypes
                 closureBindings
                 (foldr1 Lambda annotationTypes)
                 typedExpression
          else throwError $
               compileError
                 ("Expected " <> s name <> " to return type " <>
                  printType expectedReturnType <>
                  ", but instead got type " <>
                  printType (typeOf typedExpression))
  (runClosureBindings actualReturnType) >>= typeChecks
  where
    compileError = CompileError (DeclarationError declaration) position
    makeDeclarations :: TypedArgument -> [TypedDeclaration]
    makeDeclarations typedArgument =
      case typedArgument of
        TAIdentifier t i -> [makeDeclaration t i]
        TADeconstruction _ (ConstructorSymbol constructor) _ args ->
          let declaration = find m (concat . Map.elems $ typeConstructors state)
              m (TypedConstructor name _ _) = name == constructor -- TODO - should probably match on types as well!
              declarations (TypedConstructor _ _ _) =
                concatMap makeDeclarations $ args
           in maybe [] declarations declaration
        TANumberLiteral _ -> []
    makeDeclaration :: Type -> Symbol -> TypedDeclaration
    makeDeclaration t i =
      TypedDeclaration i [] OSet.empty t (TypeChecker.Identifier t (Binding Local i) OSet.empty)

lambdaType :: Type -> Type -> [Type] -> Type
lambdaType left right remainder =
  case remainder of
    [] -> Lambda left right
    (x:xs) -> Lambda left (lambdaType right x xs)

typeOf :: TypedExpression -> Type
typeOf t =
  case t of
    TypeChecker.Identifier t _ _ -> t
    TypeChecker.Apply t _ _ -> t
    TypeChecker.Number _ -> Num
    TypeChecker.Float _ -> Float'
    TypeChecker.Infix t _ _ _ -> t
    TypeChecker.Case t _ _ -> t
    TypeChecker.Let _ te -> typeOf te
    TypeChecker.String' _ -> Str
    TypeChecker.ADTConstruction _ _ _ -> Lambda Num Num -- TODO - make this real

inferApplicationType ::
     CompileState
  -> Expression
  -> Expression
  -> (Expression -> Maybe SourceRange)
  -> (Text -> CompileError)
  -> DeclarationCompilation TypedExpression
inferApplicationType state a b exprPosition compileError =
  let typedExprs =
        (,) <$> inferType state a exprPosition <*>
        inferType state b exprPosition
      inferApplication ::
           (TypedExpression, TypedExpression)
        -> DeclarationCompilation TypedExpression
      inferApplication (a, b) =
        case (typeOf a, typeOf b) of
          (Lambda x r, b') ->
            case typeConstraints x b' of
              Just constraints ->
                return
                  (TypeChecker.Apply (replaceGenerics constraints r) a b)
              Nothing ->
                throwError $
                compileError
                  ("Function expected argument of type " <> printType x <>
                   ", but instead got argument of type " <>
                   printType b')
          _ ->
            throwError $
            compileError $
            "Tried to apply a value of type " <> printType (typeOf a) <>
            " to a value of type " <>
            printType (typeOf b)
   in typedExprs >>= inferApplication

inferIdentifierType ::
     CompileState
  -> Ident
  -> FunctionType
  -> (Text -> CompileError)
  -> DeclarationCompilation TypedExpression
inferIdentifierType state name _ compileError =
  case find (m name) declarations of
    Just (Closure, TypedDeclaration s _ bindings t _) -> do
      _ <- addClosureBinding $ ClosureBinding s t
      _ <- sequence_ $ addClosureBinding <$> OSet.toAscList bindings
      return $ TypeChecker.Identifier t (Binding Closure s) bindings
    Just (scope, TypedDeclaration s _ bindings t _) -> do
      _ <- sequence_ $ addClosureBinding <$> OSet.toAscList bindings
      return $ TypeChecker.Identifier t (Binding scope s) bindings
    Nothing ->
      throwError $
      compileError
        ("It's not clear what \"" <> idToString name <> "\" refers to")
  where
    declarations = typedDeclarations state
    m name (_, TypedDeclaration (Symbol _ name') _ _ _ _) = name == name'

inferInfixType ::
     CompileState
  -> OperatorExpr
  -> Expression
  -> Expression
  -> (Expression -> Maybe SourceRange)
  -> (Text -> CompileError)
  -> DeclarationCompilation TypedExpression
inferInfixType state op a b exprPosition compileError =
  let validInfix a b =
        case (op, b, typeEq a b) of
          (StringAdd, Str, True) -> Just Str
          (StringAdd, _, _) -> Nothing
          (_, Num, True) -> Just Num
          (_, Float', True) -> Just Float'
          (_, _, _) -> Nothing
      types =
        (,) <$> inferType state a exprPosition <*>
        inferType state b exprPosition
      checkInfix ::
           (TypedExpression, TypedExpression)
        -> DeclarationCompilation TypedExpression
      checkInfix (a, b) =
        case validInfix (typeOf a) (typeOf b) of
          Just returnType -> return $ (TypeChecker.Infix returnType op a b)
          Nothing ->
            throwError $
            compileError
              ("No function exists with type " <> printType (typeOf a) <> " " <>
               operatorToString op <>
               " " <>
               printType (typeOf b))
   in types >>= checkInfix

inferCaseType ::
     CompileState
  -> Expression
  -> (NonEmpty (Argument, Expression))
  -> (Expression -> Maybe SourceRange)
  -> (Text -> CompileError)
  -> DeclarationCompilation TypedExpression
inferCaseType state value branches exprPosition compileError = do
  typedValue <- inferType state value exprPosition
  typedBranches <- sequence $ inferBranch typedValue <$> branches
  allBranchesHaveSameType typedValue typedBranches
  where
    inferBranch v (a, b) = do
      a' <-
        liftCompilationState $ inferArgumentType state compileError (typeOf v) a
      let argDeclarations = declarationsFromTypedArgument a'
      b' <- inferType (addLocals state argDeclarations) b exprPosition
      return (a', b')
    allBranchesHaveSameType ::
         TypedExpression
      -> NonEmpty (TypedArgument, TypedExpression)
      -> DeclarationCompilation TypedExpression
    allBranchesHaveSameType value types =
      case NE.groupWith (typeOf . snd) types of
        [x] -> return (TypeChecker.Case (typeOf . snd $ NE.head x) value types)
        -- TODO - there is a bug where we consider Result a b to be equal to Result c d,
        --        failing to recognize the importance of whether a and b have been bound in the signature
        types' ->
          if all
               (\case
                  (x:y:_) -> x `typeEq` y || y `typeEq` x
                  _ -> False)
               (F.toList <$> replicateM 2 (typeOf . snd . NE.head <$> types'))
            then return $
                 (TypeChecker.Case
                    (typeOf . snd $ NE.head (head types'))
                    value
                    types)
            else throwError $
                 compileError
                   ("Case expression has multiple return types: " <>
                    T.intercalate
                      ", "
                      (printType <$> NE.toList (typeOf . snd <$> types)))

inferLetType ::
     CompileState
  -> NonEmpty Declaration
  -> Expression
  -> (Expression -> Maybe SourceRange)
  -> (Text -> CompileError)
  -> DeclarationCompilation TypedExpression
inferLetType state declarations' value exprPosition _ =
  let branchTypes ::
           [TypedDeclaration]
        -> [Declaration]
        -> CompilationSymbols [TypedDeclaration]
      branchTypes typed untyped =
        case untyped of
          [] -> return []
          (x:xs) ->
            let concatBranchTypes t = (:) t <$> branchTypes (typed ++ [t]) xs
                inferBranchType =
                  checkDeclaration
                    (markClosureBoundary (addLocals state typed))
                    x
                    Nothing
                    exprPosition
             in inferBranchType >>= concatBranchTypes
      types :: DeclarationCompilation [TypedDeclaration]
      types = liftCompilationState $ branchTypes [] (NE.toList declarations')
      expression :: [TypedDeclaration] -> DeclarationCompilation TypedExpression
      expression b =
        (TypeChecker.Let (NE.fromList b) <$>
         inferType (addLocals state b) value exprPosition)
   in types >>= expression

inferType ::
     CompileState
  -> Expression
  -> (Expression -> Maybe SourceRange)
  -> CompilationSymbolsT ClosureBindingsT TypedExpression
inferType state expr exprPosition =
  case expr of
    Language.Number n -> return $ TypeChecker.Number n
    Language.Float f -> return $ TypeChecker.Float f
    Language.String' s -> return $ TypeChecker.String' s
    Language.BetweenParens expr -> inferType state expr exprPosition
    Language.Identifier name ->
      inferIdentifierType state name functionType compileError
      where
        functionType = Standard
    Language.Apply a b ->
      inferApplicationType state a b exprPosition compileError
    Language.Infix op a b ->
      inferInfixType state op a b exprPosition compileError
    Language.Case value branches ->
      inferCaseType state value branches exprPosition compileError
    Language.Let declarations' value ->
      inferLetType state declarations' value exprPosition compileError
  where
    compileError = CompileError (ExpressionError expr) (exprPosition expr)

inferArgumentType ::
     CompileState
  -> (Text -> CompileError)
  -> Type
  -> Argument
  -> CompilationSymbols TypedArgument
inferArgumentType state err valueType arg =
  case arg of
    AIdentifier i -> do
      symbol <- getSymbol i
      return $ TAIdentifier valueType symbol
    ANumberLiteral i ->
      if valueType == Num
        then return $ TANumberLiteral i
        else throwError $
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
            (\typeLambdaName ->
               find
                 (\(TypeLambda name') -> typeLambdaName == name')
                 (typeLambdas state))
          constructorsForValue =
            typeLambda >>= flip Map.lookup (typeConstructors state)
          matchingConstructor =
            find (m name) (fromMaybe [] constructorsForValue)
          m name (TypedConstructor (Symbol _ name') _ _) = name == name'
          deconstructionFields fields =
            sequence $
            (\(a, t) -> inferArgumentType state err t a) <$> zip args fields
       in case matchingConstructor of
            Just (TypedConstructor ctSymbol@(Symbol _ name) tag fields) -> do
              symbol <- getSymbol name
              if length args == length fields
                then TADeconstruction symbol (ConstructorSymbol ctSymbol) tag <$>
                     deconstructionFields fields
                else throwError $
                     err $
                     "Expected " <> s name <> " to have " <> showT (fields) <>
                     " fields, instead found " <>
                     showT (args) <>
                     " arg: " <>
                     showT (arg)
                     -- TODO - make this error message prettier
            Nothing ->
              throwError $
              err $
              "no constructor named \"" <> s name <> "\" for " <>
              printType valueType <>
              " in scope."

inferDeclarationType ::
     CompileState
  -> Declaration
  -> Maybe SourceRange
  -> CompilationSymbols (NE.NonEmpty Type)
inferDeclarationType state declaration lineInformation =
  case annotation of
    Just (Annotation _ types) -> annotationStructureToTypes types
    Nothing -> throwError $ compileError "For now, annotations are required."
  where
    (Declaration annotation _ _ _) = declaration
    compileError :: Text -> CompileError
    compileError = CompileError (DeclarationError declaration) lineInformation
    annotationStructureToTypes :: AnnotationStructure -> CompilationSymbols (NE.NonEmpty Type)
    annotationStructureToTypes (AReturn aType) = pure <$> annotationTypeToType aType
    annotationStructureToTypes (ALambda _ aType innerStructure) =
      NE.cons <$> annotationTypeToType aType <*> annotationStructureToTypes innerStructure

    annotationTypeToType :: AnnotationType -> CompilationSymbols Type
    annotationTypeToType t =
      case t of
        Concrete i -> findTypeFromIdent (types state) compileError i
        Parenthesized types -> reduceTypes types
        TypeApplication a b -> inferTypeApplication a b
      where
        m name (TypeLambda name') = name == name'
        inferTypeApplication ::
             AnnotationType -> AnnotationType -> CompilationSymbols Type
        inferTypeApplication a b =
          case a of
            Concrete i ->
              case find (m i) (typeLambdas state) of
                Just typeLambda ->
                  Applied (TL typeLambda) <$> annotationTypeToType b
                Nothing ->
                  throwError $
                  compileError $ "Could not find type lambda: " <> idToString i
            Parenthesized a' ->
              Applied <$> reduceTypes a' <*> annotationTypeToType b
            TypeApplication a' b' ->
              Applied <$> inferTypeApplication a' b' <*> annotationTypeToType b
    reduceTypes :: AnnotationStructure -> CompilationSymbols Type
    reduceTypes (AReturn aType) = annotationTypeToType aType
    reduceTypes (ALambda _ aType as) = Lambda <$> (annotationTypeToType aType) <*> (reduceTypes as)

collapseTypes :: NE.NonEmpty Type -> Type
collapseTypes = foldr1 Lambda

declarationsFromTypedArgument :: TypedArgument -> [TypedDeclaration]
declarationsFromTypedArgument ta =
  case ta of
    TAIdentifier t n -> [TypedDeclaration n [] OSet.empty t (TypeChecker.Number 0)]
    TANumberLiteral _ -> []
    TADeconstruction _ _ _ args -> concatMap declarationsFromTypedArgument args

findTypeFromIdent ::
     Map Ident Type
  -> (Text -> CompileError)
  -> Ident
  -> CompilationSymbols Type
findTypeFromIdent types compileError ident =
  if T.toLower i == i
    then return $ Generic ident
    else case Map.lookup ident types of
           Just t -> return t
           Nothing ->
             throwError $
             compileError $ "Could not find type " <> s ident <> "."
  where
    i = s ident

printType :: Type -> Text
printType t =
  case t of
    Str -> "String"
    Num -> "Int"
    Float' -> "Float"
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
    Float' -> f t
    Str -> f t
    Lambda a b -> f (Lambda (mapType f a) (mapType f b))
    Applied typeLambda t -> f (Applied typeLambda (mapType f t))
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

symbolToText :: Symbol -> Text
symbolToText (Symbol _ i) = s i
