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
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import Data.Text (Text)

import HaskellSyntax
import qualified Language as L
import qualified TypeChecker as T

data Module =
  Module [Declaration]
  deriving (Eq, Show)

data Declaration =
  Declaration T.Symbol
              [Arg]
              Type
              [Statement]
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

data SetField
  = SetFieldFromSymbol Int
                       T.Type
                       T.Symbol
  | SetFieldFromInt Int
                    Int
  deriving (Eq, Show)

--
-- TODO - more lol
data Statement
  = Deconstruct T.Symbol
  | SetLocal T.Symbol
  | Allocate Int
             [SetField]
  | LoadLocal T.Symbol
  | LocalDeclaration Declaration
  | CallDeclaration T.Symbol
  | NumericOperation Operation
  | PushLiteral Literal
  | StringAdd
  deriving (Eq, Show)

compileDeclaration :: T.TypedDeclaration -> Declaration
compileDeclaration (T.TypedDeclaration name args _ t typedExpression) =
  Declaration name (mapMaybe compileArg args) returnType statements
  where
    statements = deconstructions <> body
    deconstructions :: [Statement]
    deconstructions = concatMap compileDeconstruction args
    body :: [Statement]
    body = compileExpression typedExpression
    returnType :: Type
    returnType = typeCast t

typeCast :: T.Type -> Type
typeCast t =
  case t of
    T.Float' -> Float
    _ -> Int

compileExpression :: T.TypedExpression -> [Statement]
compileExpression t =
  case t of
    T.Identifier _type (T.Binding scope symbol) _ ->
      case scope of
        T.Global -> [CallDeclaration symbol]
        _ -> [LoadLocal symbol]
    T.Number n -> [PushLiteral (LitInt n)]
    T.Float n -> [PushLiteral (LitFloat n)]
    T.Infix _type operator a b ->
      compileExpression a <> compileExpression b <> operation
      where operation =
              case operator of
                L.StringAdd -> [StringAdd]
                L.Add -> [NumericOperation Add]
                L.Subtract -> [NumericOperation Subtract]
                L.Multiply -> [NumericOperation Multiply]
                L.Divide -> [NumericOperation Divide]
    T.String' str -> [PushLiteral (LitString str)]
    T.ADTConstruction tag _ args ->
      let setField (offset, typedArg) =
            case typedArg of
              T.TAIdentifier t i -> SetFieldFromSymbol (offset * 4) t i
              T.TANumberLiteral n -> SetFieldFromInt (offset * 4) n
              T.TADeconstruction _ _ _ _ ->
                error
                  "I got lazy and that's why this crashes - memory manger ADTConstruction deconstruction"
          fieldsFromArgs = setField <$> (zip [1 ..] args)
          fields = [SetFieldFromInt 0 tag] <> fieldsFromArgs
          size = (length fields) * 4
       in [Allocate size fields]
    T.Let declarations expression ->
      let
        declarationStatements :: [Declaration]
        declarationStatements = compileDeclaration <$> NE.toList declarations
        localDeclarations = LocalDeclaration <$> declarationStatements
        expressionStatements = compileExpression expression
      in
        localDeclarations <> expressionStatements
    T.Apply _ _ _ -> undefined
    T.Case _ _ _ -> undefined


compileDeconstruction :: T.TypedArgument -> [Statement]
compileDeconstruction (T.TAIdentifier _ _) = []
compileDeconstruction (T.TANumberLiteral _) = []
compileDeconstruction (T.TADeconstruction symbol _ _ _) = [Deconstruct symbol]

compileArg :: T.TypedArgument -> Maybe Arg
compileArg (T.TAIdentifier _type symbol) = Just $ Arg symbol (typeCast _type)
compileArg (T.TANumberLiteral _) = Nothing
compileArg (T.TADeconstruction symbol _ _ _) = Just $ Arg symbol Int

-- If
--compileIdentifer :: Type -> F.Ident -> Set F.Ident -> Expression
--compileIdentifer t i l =
--  case t of
--    T.Applied (T.TL (T.TypeLambda _)) (T.Generic (F.Ident _)) ->
--      if (Set.member i l)
--        then GetLocal i
--        else NamedCall i []
--    T.TL (T.TypeLambda _) ->
--      if (Set.member i l)
--        then GetLocal i
--        else NamedCall i []
--    _ -> GetLocal i
compile :: T.TypedModule -> Module
compile (T.TypedModule declarations) =
  Module $ compileDeclaration <$> declarations
