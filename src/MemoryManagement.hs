{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module MemoryManagement
  ( compile
  , Module(..)
  , Declaration(..)
  , Arg(..)
  , Type(..)
  , Statement(..)
  , Literal(..)
  , Operation(..)
  , SetField(..)
  , FieldValue(..)
  , Pattern(..)
  , Deconstruction(..)
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import Data.Set.Ordered (OSet)
import qualified Data.Set.Ordered as OSet
import Data.Text (Text)

import Debug.Trace (trace)
import HaskellSyntax
import qualified Language as L
import qualified TypeChecker as T

data Module =
  Module [Declaration]
  deriving (Eq, Show)

data Declaration =
  Declaration T.Symbol
              [Arg]
              [Arg]
              Type
              (NonEmpty Statement)
  deriving (Eq, Show)

data Arg =
  Arg T.Symbol
      Type
  deriving (Eq, Show)

data Type
  = Int
  | Float
  deriving (Eq, Show)

data Literal
  = LitInt Int
  | LitFloat Float
  | LitString Text
  deriving (Eq, Show)

data Operation
  = Add
  | Subtract
  | Divide
  | Multiply
  deriving (Eq, Show)

data SetField =
  SetField Int
           FieldValue
  deriving (Eq, Show)

data FieldValue
  = FieldFromSymbol Type
                    T.Symbol
  | FieldFromInt Int
  deriving (Eq, Show)

data Pattern
  = ExactMatch Int
               [Deconstruction]
  | NumericMatch Int
  | Wildcard T.Symbol
  deriving (Eq, Show)

data Deconstruction =
  Deconstruction T.Symbol
                 T.Symbol
                 T.ConstructorSymbol
                 Int
                 Type
  deriving (Eq, Show)

-- TODO - more lol
data Statement
  = LoadLocal T.Symbol
  | PushLiteral Literal
  | StringAdd
  | Allocate Int
             [SetField]
  | LocalDeclaration Declaration
  | CallDeclaration T.Symbol
  | NumericOperation Type
                     Operation
  | Deconstruct Deconstruction
  | Case Type
         (NonEmpty Statement)
         (NonEmpty (Pattern, NonEmpty Statement))
  deriving (Eq, Show)

data FunctionLevel
  = TopLevel
  | Inline

compileDeclaration :: FunctionLevel -> T.TypedDeclaration -> Declaration
compileDeclaration functionLevel (T.TypedDeclaration name args closureBindings t typedExpression) =
  Declaration name argsFromClosureBindings (mapMaybe compileArg args) returnType statements
  where
    argsFromClosureBindings =
      case functionLevel of
        TopLevel -> []
        Inline -> (closureArgs closureBindings)
    statements = NE.fromList (deconstructions <> NE.toList body)
    deconstructions = Deconstruct <$> concatMap compileDeconstruction args
    body = compileExpression typedExpression
    returnType :: Type
    returnType = typeCast $ findReturnType t
    findReturnType t =
      case t of
        T.Lambda _ b -> findReturnType b
        T.Applied _ b -> findReturnType b
        _ -> t

typeCast :: T.Type -> Type
typeCast t =
  case t of
    T.Float' -> Float
    _ -> Int

compileExpression :: T.TypedExpression -> NonEmpty Statement
compileExpression t =
  case t of
    T.Identifier _type (T.Binding scope symbol) _ ->
      case (_type, scope) of
        (_, T.Global) -> [CallDeclaration symbol]
        (T.Lambda _ _, _) -> [CallDeclaration symbol]
        _ -> [LoadLocal symbol]
    T.Number n -> [PushLiteral (LitInt n)]
    T.Float n -> [PushLiteral (LitFloat n)]
    T.Infix resultType operator a b ->
      compileExpression a <> compileExpression b <> operation
      where numeric = NumericOperation (typeCast resultType)
            operation =
              case operator of
                L.StringAdd -> [StringAdd]
                L.Add -> [numeric Add]
                L.Subtract -> [numeric Subtract]
                L.Multiply -> [numeric Multiply]
                L.Divide -> [numeric Divide]
    T.String' str -> [PushLiteral (LitString str)]
    T.ADTConstruction tag _ args ->
      let setField (offset, typedArg) =
            case typedArg of
              T.TAIdentifier t i ->
                SetField offset (FieldFromSymbol (typeCast t) i)
              T.TANumberLiteral n -> SetField offset (FieldFromInt n)
              T.TADeconstruction _ _ _ _ ->
                error
                  "I got lazy and that's why this crashes - memory manger ADTConstruction deconstruction"
          fieldsFromArgs = setField <$> (zip offsets args)
          fields = [SetField 0 (FieldFromInt tag)] <> fieldsFromArgs
          size = (length fields) * 4
       in [Allocate size fields]
    T.Let declarations expression ->
      let declarationStatements :: [Declaration]
          declarationStatements =
            compileDeclaration Inline <$> NE.toList declarations
          localDeclarations = LocalDeclaration <$> declarationStatements
          expressionStatements = compileExpression expression
       in NE.fromList (localDeclarations <> NE.toList expressionStatements)
    (T.Apply _ left right) -> compileApply [] left right
    T.Case t typedExpression patterns ->
      [ Case
          (typeCast t)
          (compileExpression typedExpression)
          (compilePattern <$> patterns)
      ]
      where compilePattern (arg, expression) =
              case arg of
                T.TAIdentifier _ s ->
                  (Wildcard s, (compileExpression expression))
                T.TANumberLiteral n ->
                  (NumericMatch n, (compileExpression expression))
                T.TADeconstruction _ _ tag _ ->
                  ( ExactMatch tag (compileDeconstruction arg)
                  , (compileExpression expression))

compileApply ::
     [Statement] -> T.TypedExpression -> T.TypedExpression -> NonEmpty Statement
compileApply applied left right =
  case left of
    T.Apply _ left' right' ->
      compileApply
        ((NE.toList (compileExpression right)) <> applied)
        left'
        right'
    T.Identifier _ (T.Binding _ name) closureBindings ->
      let rightStatements = compileExpression right
          closureStatements = bindingsToArgs closureBindings
       in NE.fromList $
          closureStatements <> NE.toList rightStatements <> applied <>
          [CallDeclaration name]
    _ -> compileExpression left <> compileExpression right

closureArgs :: OSet T.ClosureBinding -> [Arg]
closureArgs cbs = mapMaybe closureArg $ OSet.toAscList cbs
  where
    closureArg (T.ClosureBinding _ (T.Lambda _ _)) = Nothing
    closureArg (T.ClosureBinding s t) = Just $ Arg s (typeCast t)

bindingsToArgs :: OSet T.ClosureBinding -> [Statement]
bindingsToArgs cbs = mapMaybe bindingToArg $ OSet.toAscList cbs

bindingToArg :: T.ClosureBinding -> Maybe Statement
bindingToArg (T.ClosureBinding _ (T.Lambda _ _)) = Nothing
bindingToArg (T.ClosureBinding s _) = Just $ LoadLocal s

offsets :: [Int]
offsets = (* 4) <$> [1 ..]

-- TODO deconstruction args
compileDeconstruction :: T.TypedArgument -> [Deconstruction]
compileDeconstruction (T.TAIdentifier _ _) = []
compileDeconstruction (T.TANumberLiteral _) = []
compileDeconstruction (T.TADeconstruction symbol ctorSymbol _ args) =
  concat $ compileDeconstructionArgs symbol ctorSymbol <$> zip args offsets

compileDeconstructionArgs ::
     T.Symbol
  -> T.ConstructorSymbol
  -> (T.TypedArgument, Int)
  -> [Deconstruction]
compileDeconstructionArgs bindingSym ctorSym ((T.TAIdentifier t ident), offset) =
  [Deconstruction ident bindingSym ctorSym offset (typeCast t)]
compileDeconstructionArgs _ _ ((T.TANumberLiteral _), _) = []
compileDeconstructionArgs bindingSym ctorSym ((T.TADeconstruction symbol nestedCtorSymbol _ args), offset) =
  [Deconstruction symbol bindingSym ctorSym offset Int] <>
  (concat $
   (compileDeconstructionArgs symbol nestedCtorSymbol <$> zip args offsets))

compileArg :: T.TypedArgument -> Maybe Arg
compileArg (T.TAIdentifier _type symbol) = Just $ Arg symbol (typeCast _type)
compileArg (T.TANumberLiteral _) = Nothing
compileArg (T.TADeconstruction symbol _ _ _) = Just $ Arg symbol Int

compile :: T.TypedModule -> Module
compile (T.TypedModule declarations) =
  Module $ compileDeclaration TopLevel <$> declarations
