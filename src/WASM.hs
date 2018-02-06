{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}

module WASM
  ( Expression(..)
  , Module(..)
  , printWasm
  , forestExprToWasm
  , forestModuleToWasm
  ) where

import qualified HaskellSyntax as F

import Control.Arrow ((***))
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Generics.Deriving as G

newtype Module =
  Module [Expression]

data Declaration =
  Declaration F.Ident
              [F.Ident]
              Expression
  deriving (Show, Eq, G.Generic)

data Expression
  = Const Int
  | Func Declaration
  | GetLocal F.Ident
  | SetLocal F.Ident
             Expression
  | Call F.Ident
         [Expression]
  | NamedCall F.Ident
              [Expression]
  | If Expression
       Expression
       (Maybe Expression)
  | Sequence (NE.NonEmpty Expression)
  deriving (Show, Eq)

indent :: Int -> String -> String
indent level str =
  intercalate "\n" $ map (\line -> replicate level ' ' ++ line) (lines str)

indent2 :: String -> String
indent2 = indent 2

forestModuleToWasm :: F.Module -> Module
forestModuleToWasm (F.Module declarations) =
  Module (forestDeclarationToWasm True <$> declarations)

forestDeclarationToWasm :: Bool -> F.Declaration -> Expression
forestDeclarationToWasm topLevel (F.Declaration name args fexpr) =
  case (args, topLevel) of
    ([], False) -> SetLocal name expr'
    _ -> Func (Declaration name args expr')
  where
    expr' = forestExprToWasm fexpr

forestExprToWasm :: F.Expression -> Expression
forestExprToWasm fexpr =
  case fexpr of
    F.Identifier i -> GetLocal i
    F.Number n -> Const n
    F.BetweenParens fexpr -> forestExprToWasm fexpr
    F.Infix operator a b ->
      Call (funcForOperator operator) [forestExprToWasm a, forestExprToWasm b]
    F.Call name arguments -> NamedCall name (map forestExprToWasm arguments)
    F.Case caseFexpr patterns ->
      constructCase (forestExprToWasm caseFexpr) (patternsToWasm patterns)
    F.Let declarations fexpr ->
      Sequence $
      (forestDeclarationToWasm False <$> declarations) <>
      [forestExprToWasm fexpr]
  where
    constructCase ::
         Expression -> NE.NonEmpty (Expression, Expression) -> Expression
    constructCase caseExpr patterns =
      case patterns of
        [x] -> If (Call eq32 [caseExpr, fst x]) (snd x) Nothing
        (x :| xs) ->
          If
            (Call eq32 [caseExpr, fst x])
            (snd x)
            (Just (constructCase caseExpr (NE.fromList xs)))
    patternsToWasm = fmap (forestExprToWasm *** forestExprToWasm)

eq32 :: F.Ident
eq32 = F.Ident . F.NonEmptyString $ NE.fromList "i32.eq"

funcForOperator :: F.OperatorExpr -> F.Ident
funcForOperator operator =
  F.Ident . F.NonEmptyString $
  NE.fromList $
  case operator of
    F.Add -> "i32.add"
    F.Subtract -> "i32.sub"
    F.Multiply -> "i32.mul"
    F.Divide -> "i32.div_s"

printWasm :: Module -> String
printWasm (Module expressions) =
  "(module\n" ++
  indent2 (intercalate "\n" $ map (printWasmExpr' True) expressions) ++ "\n)"

printWasmExpr' :: Bool -> Expression -> String
printWasmExpr' topLevel expr =
  case expr of
    Sequence exprs -> intercalate "\n" $ NE.toList (printWasmExpr <$> exprs)
    Const n -> "(i32.const " ++ show n ++ ")"
    GetLocal name -> "(get_local $" ++ F.s name ++ ")"
    SetLocal name expr' ->
      "(set_local $" ++ F.s name ++ " " ++ printWasmExpr expr' ++ ")"
    Call name args ->
      "(" ++
      F.s name ++ "\n" ++ indent2 (unlines (printWasmExpr <$> args)) ++ "\n)"
    NamedCall name args ->
      "(call $" ++
      F.s name ++ "\n" ++ indent2 (unlines (printWasmExpr <$> args)) ++ "\n)"
    If conditional a b ->
      unlines
        ([ "(if (result i32)"
         , indent2 $ printWasmExpr conditional
         , indent2 $ printWasmExpr a
         ] <>
         [indent2 $ maybe "(i32.const 0)" printWasmExpr b, ")"])
    Func (Declaration name args body) ->
      case topLevel of
        True ->
          unlines
            [ unlines $ printDeclaration <$> innerFunctions body
            , "(export \"" ++ F.s name ++ "\" (func $" ++ F.s name ++ "))"
            , printDeclaration (Declaration name args body)
            ]
        False -> ""
  where
    innerFunctions :: Expression -> [Declaration]
    innerFunctions expr =
      case expr of
        Sequence exprs -> concatMap innerFunctions exprs
        Func (Declaration name args body) ->
          [Declaration name args body] <> innerFunctions body
        If cond a b ->
          innerFunctions cond <> innerFunctions a <>
          (fromMaybe [] $ innerFunctions <$> b)
        SetLocal _ expr -> innerFunctions expr
        _ -> []

printWasmExpr :: Expression -> String
printWasmExpr = printWasmExpr' False

printDeclaration :: Declaration -> String
printDeclaration (Declaration name args body) =
  intercalate
    "\n"
    [ "(func $" ++
      F.s name ++
      unwords (map (\x -> " (param $" ++ x ++ " i32)") (F.s <$> args)) ++
      " (result i32) " ++ unwords (printLocal <$> locals body)
    , indent2 $ unlines ["(return", indent2 $ printWasmExpr body, ")"]
    , ")"
    ]
  where
    locals :: Expression -> [String]
    locals expr' =
      case expr' of
        SetLocal name _ -> [F.s name]
        Sequence exprs -> concatMap locals $ NE.toList exprs
        _ -> []
    -- TODO - there are probably other important cases we should handle here

printLocal :: String -> String
printLocal name = "(local $" ++ name ++ " i32)"
