{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}

module Wasm
  ( Expression(..)
  , Module(..)
  , WasmType(..)
  , Declaration(..)
  , TopLevel(..)
  , UniqueLocals(..)
  , printWasm
  , forestModuleToWasm
  , assignments
  , ident
  ) where

import qualified Language as F

import Control.Arrow ((***))
import Control.Monad.Trans.State.Lazy
import Data.Char
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Semigroup ((<>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Generics.Deriving as G
import Text.RawString.QQ

import TypeChecker
import qualified TypeChecker as T

type BytesAllocated = Int

data Module =
  Module [TopLevel]
         BytesAllocated
  deriving (Show, Eq)

data WasmType
  = I32
  | F32
  deriving (Show, Eq, G.Generic)

data Declaration =
  Declaration F.Ident
              [(F.Ident, WasmType)]
              WasmType
              Expression
  deriving (Show, Eq, G.Generic)

data TopLevel
  = Func Declaration
  | Data Int
         Text
  deriving (Show, Eq)

data Expression
  = Const Int
  | FloatConst Float
  | GetLocal F.Ident
  | SetLocal F.Ident
             WasmType
             Expression
  | Call F.Ident
         [Expression]
  | NamedCall F.Ident
              [Expression]
  | If Expression
       Expression
       (Maybe Expression)
  | Sequence WasmType
             (NE.NonEmpty Expression)
  deriving (Show, Eq)

newtype Locals =
  Locals (Set F.Ident)

newtype UniqueLocals =
  UniqueLocals (Map F.Ident Int)

noLocals :: Locals
noLocals = Locals (Set.empty)

addLocal :: F.Ident -> Locals -> Locals
addLocal i (Locals l) = Locals (Set.insert i l)

addLocals :: [F.Ident] -> Locals -> Locals
addLocals is (Locals l) = Locals (Set.union l (Set.fromList is))

mergeLocals :: Locals -> Locals -> Locals
mergeLocals (Locals a) (Locals b) = Locals (Set.union a b)

showT :: Show a => a -> Text
showT = Text.pack . show

-- TODO - malloc could probably be better
prelude :: BytesAllocated -> Text
prelude bytesAllocated =
  let freeBlock =
        ("(global $freeblock (mut i32) (i32.const " <> showT bytesAllocated <>
         "))\n\n")
   in freeBlock <>
      [r|

(export "malloc" (func $malloc))
(func $malloc (param $size i32) (result i32)
  (local $address i32)
  (set_local $address (get_global $freeblock))
  (set_global $freeblock (i32.add (get_local $address) (get_local $size)))
  (return (get_local $address))
)

(func $string_copy (param $from i32) (param $to i32) (result i32)
  (local $index i32)
  (local $size i32)

  (set_local $index (i32.const 1))
  (set_local $size (i32.load8_u (get_local $from)))

  (loop $copy
    (i32.store8
      (i32.add (get_local $to) (get_local $index))
      (i32.load8_u (i32.add (get_local $from) (get_local $index)))
    )
    (set_local $index (i32.add (get_local $index) (i32.const 1)))
    (br_if $copy (i32.lt_s (get_local $index) (get_local $size)))
  )

  (return (get_local $size))
)

(func $string_add (param $a i32) (param $b i32) (result i32)
  (local $sum i32)
  (local $aSize i32)
  (local $newStr i32)
  (return
    (set_local $aSize (i32.load8_u (get_local $a)))
    (set_local $sum
      (i32.sub
        (i32.add
          (get_local $aSize)
          (i32.load8_u (get_local $b))
        )
        (i32.const 1)
      )
    )
    (set_local $newStr (call $malloc (i32.add (get_local $sum) (i32.const 1))))
    (i32.store8 (get_local $newStr) (get_local $sum))
    (call $string_copy (get_local $a) (get_local $newStr))
    (call $string_copy (get_local $b) (i32.sub (i32.add (get_local $newStr) (get_local $aSize)) (i32.const 1)))
    (get_local $newStr)
  )
)
|]

indent :: Int -> Text -> Text
indent level str =
  Text.intercalate "\n" $
  fmap (\line -> Text.replicate level " " <> line) (Text.lines str)

indent2 :: Text -> Text
indent2 = indent 2

forestModuleToWasm :: TypedModule -> Module
forestModuleToWasm (TypedModule topLevel) =
  foldl compileDeclaration initModule topLevel
  where
    initModule = Module [] 0

addTopLevel :: Module -> [TopLevel] -> Module
addTopLevel (Module topLevel bytes) newTopLevel =
  Module (topLevel <> newTopLevel) bytes

allocateBytes :: Module -> Int -> Module
allocateBytes (Module topLevel bytes) extraBytes =
  Module topLevel (bytes + extraBytes)

compileDeclaration :: Module -> TypedDeclaration -> Module
compileDeclaration m (TypedDeclaration name args fType fexpr) =
  let parameters = argTypes args
      deconstruction =
        evalState
          (concat <$> traverse assignments args)
          (UniqueLocals Map.empty)
      locals = Locals (Set.fromList (fst <$> parameters))
      (m', expr') = compileExpression m locals fexpr
      forestTypeToWasmType' fType =
        case fType of
          Lambda _ r -> forestTypeToWasmType' r
          Num -> I32
          Float' -> F32
          _ -> I32
      wasmType = forestTypeToWasmType' fType
      func =
        Func $
        Declaration
          (symbolToIdent name)
          parameters
          wasmType
          (Sequence wasmType $ NE.fromList (deconstruction <> [expr']))
   in addTopLevel m' [func]

compileInlineDeclaration ::
     Module -> Locals -> TypedDeclaration -> (Maybe Expression, Module)
compileInlineDeclaration m (Locals l) (TypedDeclaration name args forestType fexpr) =
  let parameters = argTypes args
      locals = Locals (Set.union l (Set.fromList (fst <$> parameters)))
      (m', expr') = compileExpression m locals fexpr
      thing =
        Func $
        Declaration (symbolToIdent name) parameters (forestTypeToWasmType forestType) expr'
   in case args of
        [] -> (Just $ SetLocal (symbolToIdent name) (forestTypeToWasmType forestType) expr', m')
        _ -> (Nothing, addTopLevel m' [thing])

forestTypeToWasmType :: T.Type -> WasmType
forestTypeToWasmType fType =
  case fType of
    Num -> I32
    Float' -> F32
    _ -> I32

compileExpressions ::
     Module -> NonEmpty TypedExpression -> (Module, [Expression])
compileExpressions m = foldl compile (m, [])
  where
    compile (m, xs) te =
      let (m', e) = compileExpression m (Locals Set.empty) te
       in (m', e : xs)

compileIdentifer :: Type -> F.Ident -> Set F.Ident -> Expression
compileIdentifer t i l =
  case t of
    T.Applied (T.TL (T.TypeLambda _)) (T.Generic (F.Ident _)) ->
      if (Set.member i l)
        then GetLocal i
        else NamedCall i []
    T.TL (T.TypeLambda _) ->
      if (Set.member i l)
        then GetLocal i
        else NamedCall i []
    _ -> GetLocal i

compileInfix ::
     Module
  -> Locals
  -> F.OperatorExpr
  -> TypedExpression
  -> TypedExpression
  -> (Module, Expression)
compileInfix m locals operator a b =
  let (m', aExpr) = compileExpression m locals a
      (m'', bExpr) = compileExpression m' locals b
      name = (F.Ident $ F.NonEmptyString 's' "tring_add")
   in case (operator, T.typeOf b) of
        (F.StringAdd, T.Str) -> (m'', NamedCall name [aExpr, bExpr])
        (_, t) -> (m'', Call (funcForOperator operator t) [aExpr, bExpr])

compileApply ::
     Module
  -> Locals
  -> TypedExpression
  -> TypedExpression
  -> (Module, Expression)
compileApply m locals left right =
  case left of
    T.Apply t (T.Identifier _ name) r' ->
      let (m', exprs) = compileExpressions m [right, r']
       in ( m'
          , Sequence (forestTypeToWasmType t) $
            NE.fromList (exprs <> [NamedCall (symbolToIdent name) []]))
    T.Identifier _ name ->
      let (m', r) = compileExpression m locals right
       in (m', NamedCall (symbolToIdent name) [r])
    _ -> error $ "do not know what to do with " <> show left

compileLet ::
     Module
  -> Locals
  -> NonEmpty TypedDeclaration
  -> TypedExpression
  -> (Module, Expression)
compileLet m locals@(Locals l) declarations fexpr =
  let compileDeclaration' ::
           (Module, [Expression]) -> TypedDeclaration -> (Module, [Expression])
      compileDeclaration' (m', declarations) declaration =
        let (mExpr, m'') = compileInlineDeclaration m' locals declaration
         in case mExpr of
              Just expr -> (m'', declarations <> [expr])
              Nothing -> (m'', declarations)
      (m', declarationExpressions) =
        foldl compileDeclaration' (m, []) declarations
      names =
        NE.toList $ (\(TypedDeclaration name _ _ _) -> name) <$> declarations
      locals' = Locals $ Set.union l (Set.fromList (symbolToIdent <$> names))
      (m'', expr') = compileExpression m' locals' fexpr
   in ( m''
      , Sequence (forestTypeToWasmType (T.typeOf fexpr)) $
        NE.fromList (declarationExpressions <> [expr']))

compileCase ::
     Module
  -> Locals
  -> TypedExpression
  -> NonEmpty (TypedArgument, TypedExpression)
  -> (Module, Expression)
compileCase m locals caseFexpr patterns =
  let (m', caseExpr) = compileCaseExpression m locals caseFexpr
      (m'', patternExprs) = patternsToWasm m' caseFexpr patterns
   in (m'', constructCase caseExpr patternExprs)
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
    patternsToWasm ::
         Module
      -> T.TypedExpression
      -> NE.NonEmpty (T.TypedArgument, T.TypedExpression)
      -> (Module, NonEmpty (Expression, Expression))
    patternsToWasm m caseFexpr patterns =
      let compilePattern ::
               (Module, [(Expression, Expression)])
            -> (T.TypedArgument, T.TypedExpression)
            -> (Module, [(Expression, Expression)])
          compilePattern (m', exprs) (a, b) =
            let (m'', aExpr, locals') = compileArgument m' caseFexpr a
                (m''', bExpr) =
                  compileExpression m'' (mergeLocals locals locals') b
             in (m''', exprs <> [(aExpr, bExpr)])
          (m', exprs) = foldl compilePattern (m, []) patterns
       in (m', NE.fromList exprs)

compileADTConstruction ::
     (Functor t, Foldable t) => Int -> t T.TypedArgument -> Expression
compileADTConstruction tag args =
  Sequence
    I32
    (NE.fromList
       ([ SetLocal
            (ident "address")
            I32
            (NamedCall (ident "malloc") [Const $ (1 + length args) * 4])
        , Call (ident "i32.store") [GetLocal (ident "address"), Const tag]
        ] <>
        (store <$> zip [1 ..] (argTypes args)) <>
        [GetLocal (ident "address")]))
  where
    store :: (Int, (F.Ident, WasmType)) -> Expression
    store (offset, (i, t)) =
      Call
        (ident (printWasmType t <> ".store"))
        [ Call
            (ident "i32.add")
            [GetLocal (ident "address"), Const (offset * 4)]
        , GetLocal i
        ]

compileString :: Module -> Text -> (Module, Expression)
compileString m str =
  let (Module _ address) = m
      m' = addTopLevel m [Data address str]
      m'' = allocateBytes m' (Text.length str + 1)
   in (m'', Const address)

compileExpression :: Module -> Locals -> TypedExpression -> (Module, Expression)
compileExpression m locals@(Locals l) fexpr =
  case fexpr of
    T.Identifier t i -> (m, compileIdentifer t (symbolToIdent i) l)
    T.Number n -> (m, Const n)
    T.Float f -> (m, FloatConst f)
    T.BetweenParens fexpr -> compileExpression m locals fexpr
    T.Infix _ operator a b -> compileInfix m locals operator a b
    T.Apply _ left right -> compileApply m locals left right
    T.Case _ caseFexpr patterns -> compileCase m locals caseFexpr patterns
    T.Let declarations fexpr -> compileLet m locals declarations fexpr
    T.String' str -> compileString m str
    T.ADTConstruction tag args -> (m, compileADTConstruction tag args)

assignments :: T.TypedArgument -> State UniqueLocals [Expression]
assignments (T.TAIdentifier _ _) = pure []
assignments (T.TADeconstruction (T.BindingSymbol i) _ _ args) =
  concat <$>
  traverse (uncurry (compileDeconstructionAssignment (symbolToIdent i))) (zip args [1 ..])
assignments (T.TANumberLiteral _) = pure []

argTypes :: Foldable t => t T.TypedArgument -> [(F.Ident, WasmType)]
argTypes = concatMap argType

argType :: T.TypedArgument -> [(F.Ident, WasmType)]
argType (T.TAIdentifier t i) = [(symbolToIdent i, forestTypeToWasmType t)]
argType (T.TANumberLiteral _) = []
argType (T.TADeconstruction (T.BindingSymbol i) _ _ _) = [(symbolToIdent i, I32)]

getUniqueLocal :: F.Ident -> State UniqueLocals F.Ident
getUniqueLocal i = do
  count <- gets countForIdent
  modify updateCount
  return $ ident ((F.s i) <> "_" <> showT count)
  where
    updateCount (UniqueLocals map) = UniqueLocals (Map.insertWith (+) i 1 map)
    countForIdent (UniqueLocals map) = fromMaybe 0 (Map.lookup i map)

compileDeconstructionAssignment ::
     F.Ident -> T.TypedArgument -> Int -> State UniqueLocals [Expression]
compileDeconstructionAssignment i a n =
  case a of
    T.TAIdentifier t symbol -> do
      return $
        [ (SetLocal
             (symbolToIdent symbol)
             (forestTypeToWasmType t)
             (Call
                (ident (printWasmType (forestTypeToWasmType t) <> ".load"))
                [Call (ident "i32.add") [GetLocal i, Const $ n * 4]]))
        ]
    T.TANumberLiteral _ -> pure []
    T.TADeconstruction (T.BindingSymbol symbol) _ _ args -> do
      let assignment =
            [ SetLocal
               (symbolToIdent symbol)
                I32
                (Call
                   (ident "i32.load")
                   [Call (ident "i32.add") [GetLocal i, Const $ n * 4]])
            ]
      ((<>) assignment) <$>
        (concat <$>
         traverse
           (uncurry (compileDeconstructionAssignment (symbolToIdent symbol)))
           (zip args [1 ..]))

compileCaseExpression ::
     Module -> Locals -> T.TypedExpression -> (Module, Expression)
compileCaseExpression m locals fexpr =
  case fexpr of
    Identifier (T.Applied _ _) i -> (m, Call (ident "i32.load") [GetLocal (symbolToIdent i)])
    Identifier (T.TL (T.TypeLambda _)) i ->
      (m, Call (ident "i32.load") [GetLocal (symbolToIdent i)])
    _ -> compileExpression m locals fexpr

compileArgument ::
     Module
  -> T.TypedExpression
  -> TypedArgument
  -> (Module, Expression, Locals)
compileArgument m caseFexpr arg =
  case arg of
    T.TAIdentifier _ i -> (m, GetLocal (symbolToIdent i), addLocal (symbolToIdent i) noLocals)
    T.TANumberLiteral n -> (m, Const n, noLocals)
    T.TADeconstruction _ _ tag args ->
      let assignments = mapMaybe makeAssignment (zip args [1 ..])
          makeAssignment :: (T.TypedArgument, Int) -> Maybe Expression
          makeAssignment (arg, index) =
            case arg of
              TAIdentifier fType symbol ->
                Just
                  (SetLocal
                     (symbolToIdent symbol)
                     (forestTypeToWasmType fType)
                     (Call
                        (ident
                           (printWasmType (forestTypeToWasmType fType) <>
                            ".load"))
                        [Call (ident "i32.add") [caseLocal, Const (index * 4)]]))
              _ -> Nothing
          localName (TAIdentifier _ symbol) = Just (symbolToIdent symbol)
          localName _ = Nothing
          locals = addLocals (mapMaybe localName args) noLocals
       in (m, Sequence I32 (NE.fromList (assignments <> [Const tag])), locals)
  where
    caseLocal =
      case caseFexpr of
        T.Identifier _ name -> GetLocal (symbolToIdent name)
        _ -> Const 0

eq32 :: F.Ident
eq32 = F.Ident $ F.NonEmptyString 'i' "32.eq"

funcForOperator :: F.OperatorExpr -> T.Type -> F.Ident
funcForOperator operator t =
  let wasmType =
        case t of
          Num -> "i32"
          Float' -> "f32"
          _ ->
            error $
            "tried to get a funcForOperator for a non numeric type: " <>
            (Text.unpack $ T.printType t)
      op =
        case (operator, t) of
          (F.Add, _) -> "add"
          (F.Subtract, _) -> "sub"
          (F.Multiply, _) -> "mul"
          (F.Divide, Float') -> "div"
          (F.Divide, _) -> "div_s"
          _ ->
            error $
            "tried to get a funcForOperator for a non numeric type: " <>
            (Text.unpack $ T.printType t)
   in ident (wasmType <> "." <> op)

printWasm :: Module -> Text
printWasm (Module expressions bytesAllocated) =
  "(module\n" <> indent2 (prelude bytesAllocated) <> "\n\n" <>
  indent2 (printMemory bytesAllocated) <>
  "\n" <>
  indent2 (Text.intercalate "\n" $ printWasmTopLevel <$> expressions) <>
  "\n)"

printMemory :: BytesAllocated -> Text
printMemory bytes =
  case bytes of
    0 -> printMemory 1 -- TODO this is silly, we should omit the prelude instead
    _ ->
      "(memory $memory " <> showT pages <>
      ")\n(export \"memory\" (memory $memory))\n\n"
  where
    pageSize = 2 ** 16
    pages = ceiling $ fromIntegral bytes / pageSize

printWasmTopLevel :: TopLevel -> Text
printWasmTopLevel topLevel =
  case topLevel of
    Func (Declaration name args wasmType body) ->
      Text.unlines
        [ "(export \"" <> F.s name <> "\" (func $" <> F.s name <> "))"
        , printDeclaration (Declaration name args wasmType body)
        ]
    Data offset str ->
      "(data (i32.const " <> showT offset <> ") \"" <>
      escape (Text.length str + 1) <>
      str <>
      "\")"
  where
    escape n =
      case n of
        0 -> "\\" <> Text.singleton (chr 0)
        34 -> "\\\""
        _ -> Text.singleton (chr n)

printWasmExpr :: Expression -> Text
printWasmExpr expr =
  case expr of
    Sequence wasmType exprs ->
      "(block (result " <> printWasmType wasmType <> ")\n" <>
      indent2 (Text.intercalate "\n" $ NE.toList (printWasmExpr <$> exprs)) <>
      "\n)"
    Const n -> "(i32.const " <> showT n <> ")"
    FloatConst n -> "(f32.const " <> showT n <> ")"
    GetLocal name -> "(get_local $" <> F.s name <> ")"
    SetLocal name _ expr' ->
      "(set_local $" <> F.s name <> " " <> printWasmExpr expr' <> ")"
    Call name args ->
      "(" <> F.s name <> "\n" <>
      indent2 (Text.intercalate "\n" (printWasmExpr <$> args)) <>
      "\n)"
    NamedCall name args ->
      "(call $" <> F.s name <> "\n" <>
      indent2 (Text.intercalate "\n" (printWasmExpr <$> args)) <>
      "\n)"
    If conditional a b ->
      Text.unlines
        ([ "(if (result i32)"
         , indent2 $ printWasmExpr conditional
         , indent2 $ printWasmExpr a
         ] <>
         [indent2 $ maybe "(i32.const 0)" printWasmExpr b, ")"])

printWasmType :: WasmType -> Text
printWasmType wasmType =
  case wasmType of
    I32 -> "i32"
    F32 -> "f32"

printDeclaration :: Declaration -> Text
printDeclaration (Declaration name args wasmType body) =
  Text.intercalate
    "\n"
    [ "(func $" <> F.s name <> Text.unwords (printParam <$> args) <> " (result " <>
      printWasmType wasmType <>
      ") " <>
      Text.unwords (printLocal <$> locals body)
    , indent2 $ Text.unlines ["(return", indent2 $ printWasmExpr body, ")"]
    , ")"
    ]
  where
    printParam (name, wasmType) =
      " (param $" <> F.s name <> " " <> printWasmType wasmType <> ")"
    locals :: Expression -> [(Text, WasmType)]
    locals expr' =
      case expr' of
        SetLocal name wasmType _ -> [(F.s name, wasmType)]
        Sequence _ exprs -> concatMap locals $ NE.toList exprs
        If expr expr' mexpr ->
          locals expr <> locals expr' <> maybe [] locals mexpr
        Call _ exprs -> concatMap locals exprs
        NamedCall _ exprs -> concatMap locals exprs
        _ -> []
    -- TODO - there are probably other important cases we should handle here

printLocal :: (Text, WasmType) -> Text
printLocal (name, wasmType) =
  "(local $" <> name <> " " <> printWasmType wasmType <> ")"

ident :: Text -> F.Ident
ident t = F.Ident $ F.NonEmptyString (Text.head t) (Text.tail t)

symbolToIdent :: T.Symbol -> F.Ident
symbolToIdent (Symbol n i) =
  ident (F.s i <> "_" <> showT n)
