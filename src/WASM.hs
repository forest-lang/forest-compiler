{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module WASM
  ( Expression(..)
  , Module(..)
  , printWasm
  , forestExprToWasm
  , forestModuleToWasm
  ) where

import qualified Lib as F

import Control.Arrow ((***))
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty(NonEmpty((:|)))
import Data.Maybe
import Data.Semigroup ((<>))
import Data.Text (Text)

newtype Module =
  Module [Expression]

data Expression
  = Const Int
  | Func F.Ident
         [F.Ident]
         Expression
  | GetLocal F.Ident
  | Call F.Ident
         [Expression]
  | NamedCall F.Ident
              [Expression]
  | If Expression
       Expression
       (Maybe Expression)

indent :: Int -> String -> String
indent level str =
  intercalate "\n" $ map (\line -> replicate level ' ' ++ line) (lines str)

indent2 :: String -> String
indent2 = indent 2

forestModuleToWasm :: F.Module -> Module
forestModuleToWasm (F.Module declarations) =
  Module (forestDeclarationToWasm <$> declarations)

forestDeclarationToWasm :: F.Declaration -> Expression
forestDeclarationToWasm (F.Declaration name args fexpr) =
  Func name args (forestExprToWasm fexpr)

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
    F.Let _ _ -> undefined
  where
    constructCase :: Expression -> NE.NonEmpty (Expression, Expression) -> Expression
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
  indent2 (intercalate "\n" $ map printWasmExpr expressions) ++ "\n)"
  where
    printWasmExpr expr =
      case expr of
        Const n -> "(i32.const " ++ show n ++ ")"
        GetLocal name -> "(get_local $" ++ F.s name ++ ")"
        Call name args ->
          "(" ++
          F.s name ++
          "\n" ++ indent2 (unlines (printWasmExpr <$> args)) ++ "\n)"
        NamedCall name args ->
          "(call $" ++
          F.s name ++
          "\n" ++ indent2 (unlines (printWasmExpr <$> args)) ++ "\n)"
        If conditional a b ->
          unlines
            ([ "(if (result i32)"
             , indent2 $ printWasmExpr conditional
             , indent2 $ printWasmExpr a
             ] <>
             [indent2 $ maybe "(i32.const 0)" printWasmExpr b, ")"])
        Func name args body ->
          unlines
            [ "(export \"" ++ F.s name ++ "\" (func $" ++ F.s name ++ "))"
            , "(func $" ++
              F.s name ++
              unwords (map (\x -> " (param $" ++ x ++ " i32)") (F.s <$> args)) ++
              " (result i32)"
            , indent2 $ unlines ["(return", indent2 $ printWasmExpr body, ")"]
            , ")"
            ]
