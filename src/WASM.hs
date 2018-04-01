{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}

module WASM
  ( Expression(..)
  , Module(..)
  , printWasm
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
  Module [TopLevel]

data Declaration =
  Declaration F.Ident
              [F.Ident]
              Expression
  deriving (Show, Eq, G.Generic)

data TopLevel
  = Func Declaration
  | Data Int
         String

data Expression
  = Const Int
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
  foldl compileDeclaration initModule declarations
  where
    initModule = Module []

compileDeclaration :: Module -> F.Declaration -> Module
compileDeclaration (Module topLevel) (F.Declaration _ name args fexpr) =
  Module (topLevel ++ [Func $ Declaration name args expr'] ++ extraTopLevel)
  where
    (expr', extraTopLevel) = compileExpression fexpr

compileInlineDeclaration :: F.Declaration -> (Maybe Expression, [TopLevel])
compileInlineDeclaration (F.Declaration _ name args fexpr) =
  case args of
    [] -> (Just $ SetLocal name expr', extraTopLevel)
    _ -> (Nothing, (Func $ Declaration name args expr') : extraTopLevel)
  where
    (expr', extraTopLevel) = compileExpression fexpr

compileExpression :: F.Expression -> (Expression, [TopLevel])
compileExpression fexpr =
  case fexpr of
    F.Identifier i -> (GetLocal i, [])
    F.Number n -> (Const n, [])
    F.BetweenParens fexpr -> compileExpression fexpr
    F.Infix operator a b ->
      let (aExpr, aTopLevel) = compileExpression a
          (bExpr, bTopLevel) = compileExpression b
       in ( Call (funcForOperator operator) [aExpr, bExpr]
          , aTopLevel ++ bTopLevel)
    F.Call name arguments ->
      let argumentExpressions = fst . compileExpression <$> arguments
          argumentTopLevel = concatMap (snd . compileExpression) arguments
       in (NamedCall name argumentExpressions, argumentTopLevel)
    F.Case caseFexpr patterns ->
      let (caseExpr, caseTopLevel) = compileExpression caseFexpr
          (patternExprs, patternTopLevel) = patternsToWasm patterns
       in ( constructCase caseExpr patternExprs
          , caseTopLevel ++
            concatMap fst (NE.toList patternTopLevel) ++
            concatMap snd (NE.toList patternTopLevel))
    F.Let declarations fexpr ->
      let declarationExpressions =
            mapMaybe (fst . compileInlineDeclaration) (NE.toList declarations)
          declarationTopLevel =
            concatMap (snd . compileInlineDeclaration) declarations
          (expr', extraTopLevel) = compileExpression fexpr
       in ( Sequence $ NE.fromList (declarationExpressions <> [expr'])
          , declarationTopLevel <> extraTopLevel)
    F.String' str -> (Const 0, [Data 0 str])
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
    patternsToWasm patterns =
      let patternExprs =
            fmap (fst . compileExpression *** fst . compileExpression) patterns
          patternTopLevel =
            fmap (snd . compileExpression *** snd . compileExpression) patterns
       in (patternExprs, patternTopLevel)

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
  indent2 (intercalate "\n" $ printWasmTopLevel <$> expressions) ++ "\n)"

printWasmTopLevel :: TopLevel -> String
printWasmTopLevel topLevel =
  case topLevel of
    Func (Declaration name args body) ->
      unlines
        [ "(export \"" ++ F.s name ++ "\" (func $" ++ F.s name ++ "))"
        , printDeclaration (Declaration name args body)
        ]
    Data offset str ->
      "(data (i32.const " ++ show offset ++ ") \"" ++ str ++ "\")"

printWasmExpr :: Expression -> String
printWasmExpr expr =
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
