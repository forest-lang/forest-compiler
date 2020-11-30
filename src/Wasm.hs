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
import Control.Monad
import Control.Monad.Trans.Class
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
import Data.Set.Ordered (OSet)
import qualified Data.Set.Ordered as OSet
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Generics.Deriving as G
import Text.RawString.QQ

import TypeChecker
import qualified TypeChecker as T

import qualified MemoryManagement as M

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
  | TeeLocal F.Ident
             WasmType
             Expression
  | Call F.Ident
         [Expression]
  | NamedCall F.Ident
              [Expression]
  | If Expression
       Expression
       (Maybe Expression)
  | Block WasmType
          (NE.NonEmpty Expression)
  | Sequence (NE.NonEmpty Expression)
  deriving (Show, Eq)

newtype Locals =
  Locals (Set F.Ident)

newtype UniqueLocals =
  UniqueLocals (Map F.Ident Int)

type CompileState = State Module

type DeclarationCompileState = StateT UniqueLocals CompileState

noLocals :: Locals
noLocals = Locals Set.empty

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
  execState (sequence $ compileDeclaration <$> topLevel) initModule
  where
    initModule = Module [] 0

getAddress :: CompileState BytesAllocated
getAddress = gets bytesAllocated
  where
    bytesAllocated (Module _ bytes) = bytes

addTopLevel :: [TopLevel] -> CompileState ()
addTopLevel newTopLevel = modify transform
  where
    transform (Module topLevel bytes) = Module (topLevel <> newTopLevel) bytes

allocateBytes :: Int -> CompileState ()
allocateBytes i = modify transform
  where
    transform (Module topLevel bytes) = Module topLevel (bytes + i)

closureArgs :: OSet ClosureBinding -> [(F.Ident, WasmType)]
closureArgs bindings = mapMaybe closureArg $ OSet.toAscList bindings

closureArg :: ClosureBinding -> Maybe (F.Ident, WasmType)
closureArg (T.ClosureBinding _ (T.Lambda _ _)) = Nothing
closureArg (T.ClosureBinding symbol fType) =
  Just (symbolToIdent symbol, forestTypeToWasmType fType)

compileDeclaration :: TypedDeclaration -> CompileState ()
compileDeclaration (TypedDeclaration name args _ fType fexpr) = do
  expr' <- evalStateT (compileExpression locals fexpr) (UniqueLocals Map.empty)
  let func =
        Func $
        Declaration
          (symbolToIdent name)
          parameters
          wasmType
          (Block wasmType $ NE.fromList (deconstruction <> [expr']))
  addTopLevel [func]
  where
    parameters = argTypes args
    deconstruction =
      evalState (concat <$> traverse assignments args) (UniqueLocals Map.empty)
    locals =
      Locals
        (Set.fromList $
         (fst <$> parameters) <> (fst <$> concatMap findLocals deconstruction))
    forestTypeToWasmType' fType =
      case fType of
        Lambda _ r -> forestTypeToWasmType' r
        Num -> I32
        Float' -> F32
        _ -> I32
    wasmType = forestTypeToWasmType' fType

compileInlineDeclaration ::
     Locals -> TypedDeclaration -> DeclarationCompileState (Maybe Expression)
compileInlineDeclaration (Locals l) (TypedDeclaration name args closureBindings forestType fexpr) = do
  expr' <- compileExpression locals fexpr
  let decl =
        inlineDeclaration expr' deconstruction (forestTypeToWasmType forestType)
  case args of
    [] ->
      return $
      (Just $
       SetLocal (symbolToIdent name) (forestTypeToWasmType forestType) expr')
    _ -> do
      lift $ addTopLevel [decl]
      return $ Nothing
  where
    deconstruction =
      evalState (concat <$> traverse assignments args) (UniqueLocals Map.empty)
    parameters = closureArgs closureBindings <> argTypes args
    locals =
      Locals
        (Set.union
           l
           (Set.fromList
              ((fst <$> parameters) <>
               (fst <$> concatMap findLocals deconstruction))))
    inlineDeclaration expr deconstruction wasmType =
      Func $
      Declaration
        (symbolToIdent name)
        parameters
        (forestTypeToWasmType forestType)
        (Block wasmType $ NE.fromList (deconstruction <> [expr]))

forestTypeToWasmType :: T.Type -> WasmType
forestTypeToWasmType fType =
  case fType of
    Num -> I32
    Float' -> F32
    _ -> I32

compileExpressions ::
     Locals -> NonEmpty TypedExpression -> DeclarationCompileState [Expression]
compileExpressions locals = foldM compile []
  where
    compile xs te = do
      expr <- compileExpression locals te
      return $ expr : xs

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
     Locals
  -> F.OperatorExpr
  -> TypedExpression
  -> TypedExpression
  -> DeclarationCompileState Expression
compileInfix locals operator a b = do
  aExpr <- compileExpression locals a
  bExpr <- compileExpression locals b
  let name = ident "string_add"
  return $
    case (operator, T.typeOf b) of
      (F.StringAdd, T.Str) -> NamedCall name [aExpr, bExpr]
      (_, t) -> Call (funcForOperator operator t) [aExpr, bExpr]

compileApply ::
     Locals
  -> TypedExpression
  -> TypedExpression
  -> DeclarationCompileState Expression
compileApply locals left right =
  case left of
    T.Apply t (T.Identifier _ name closureBindings) r' -> do
      exprs <- compileExpressions locals [right, r']
      return
        (Block (forestTypeToWasmType t) $
         NE.fromList
           (bindingsToArgs closureBindings <> exprs <>
            [NamedCall (bindingToIdent name) []]))
    T.Identifier _ name closureBindings -> do
      r <- compileExpression locals right
      return $
        NamedCall (bindingToIdent name) (bindingsToArgs closureBindings <> [r])
    _ -> error $ "do not know what to do with " <> show left
  where
    bindingsToArgs :: OSet T.ClosureBinding -> [Expression]
    bindingsToArgs cbs = mapMaybe bindingToArg $ OSet.toAscList cbs
    bindingToArg :: T.ClosureBinding -> Maybe Expression
    bindingToArg (T.ClosureBinding _ (T.Lambda _ _)) = Nothing
    bindingToArg (T.ClosureBinding s _) = Just $ GetLocal $ symbolToIdent s

compileLet ::
     Locals
  -> NonEmpty TypedDeclaration
  -> TypedExpression
  -> DeclarationCompileState Expression
compileLet (Locals l) declarations fexpr = do
  declarationExpressions <- foldM compileDeclaration' [] declarations
  expr' <- compileExpression locals' fexpr
  return
    (Block (forestTypeToWasmType (T.typeOf fexpr)) $
     NE.fromList (declarationExpressions <> [expr']))
  where
    compileDeclaration' ::
         [Expression]
      -> TypedDeclaration
      -> DeclarationCompileState [Expression]
    compileDeclaration' declarations declaration = do
      mExpr <- compileInlineDeclaration locals' declaration
      return $
        case mExpr of
          Just expr -> declarations <> [expr]
          Nothing -> declarations
    names =
      NE.toList $ (\(TypedDeclaration name _ _ _ _) -> name) <$> declarations
    locals' = Locals $ Set.union l (Set.fromList (symbolToIdent <$> names))

compileCase ::
     Locals
  -> WasmType
  -> TypedExpression
  -> NonEmpty (TypedArgument, TypedExpression)
  -> DeclarationCompileState Expression
compileCase locals _ caseFexpr patterns = do
  (resultLocal, caseExpr) <- compileCaseExpression locals caseFexpr
  patternExprs <- patternsToWasm resultLocal patterns
  return $
    Sequence [caseExpr, constructCase (GetLocal resultLocal) patternExprs]
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
         F.Ident
      -> NE.NonEmpty (T.TypedArgument, T.TypedExpression)
      -> DeclarationCompileState (NonEmpty (Expression, Expression))
    patternsToWasm caseResultIdent patterns =
      let compilePattern ::
               [(Expression, Expression)]
            -> (T.TypedArgument, T.TypedExpression)
            -> DeclarationCompileState [(Expression, Expression)]
          compilePattern exprs (a, b) = do
            (aExpr, locals') <- compileCaseArgument a
            bExpr <- compileExpression (mergeLocals locals locals') b
            return $ exprs <> [(aExpr, bExpr)]
          exprs = foldM compilePattern [] patterns
       in NE.fromList <$> exprs
      where
        compileCaseArgument a =
          case a of
            T.TAIdentifier t s ->
              return
                ( TeeLocal
                    (symbolToIdent s)
                    (forestTypeToWasmType t)
                    (GetLocal caseResultIdent)
                , addLocal (symbolToIdent s) noLocals)
            _ -> compileArgument caseFexpr a

compileADTConstruction ::
     (Functor t, Foldable t) => Int -> t T.TypedArgument -> Expression
compileADTConstruction tag args =
  Block
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

compileString :: Text -> DeclarationCompileState Expression
compileString str =
  lift $ do
    address <- getAddress
    addTopLevel [Data address str]
    allocateBytes (Text.length str + 1)
    return $ Const address

compileExpression ::
     Locals -> TypedExpression -> DeclarationCompileState Expression
compileExpression locals@(Locals l) fexpr =
  case fexpr of
    T.Identifier t i _ -> return $ compileIdentifer t (bindingToIdent i) l
    T.Number n -> return $ Const n
    T.Float f -> return $ FloatConst f
    T.Infix _ operator a b -> compileInfix locals operator a b
    T.Apply _ left right -> compileApply locals left right
    T.Case t caseFexpr patterns ->
      compileCase locals (forestTypeToWasmType t) caseFexpr patterns
    T.Let declarations fexpr -> compileLet locals declarations fexpr
    T.String' str -> compileString str
    T.ADTConstruction tag _ args -> return $ compileADTConstruction tag args

assignments :: T.TypedArgument -> State UniqueLocals [Expression]
assignments (T.TAIdentifier _ _) = pure []
assignments (T.TADeconstruction (i) _ _ args) =
  concat <$>
  traverse
    (uncurry (compileDeconstructionAssignment (symbolToIdent i)))
    (zip args [1 ..])
assignments (T.TANumberLiteral _) = pure []

argTypes :: Foldable t => t T.TypedArgument -> [(F.Ident, WasmType)]
argTypes = concatMap argType

argType :: T.TypedArgument -> [(F.Ident, WasmType)]
argType (T.TAIdentifier t i) = [(symbolToIdent i, forestTypeToWasmType t)]
argType (T.TANumberLiteral _) = []
argType (T.TADeconstruction (i) _ _ _) =
  [(symbolToIdent i, I32)]

getUniqueLocal :: Monad a => F.Ident -> StateT UniqueLocals a F.Ident
getUniqueLocal i = do
  count <- gets countForIdent
  modify updateCount
  return $ ident (F.s i <> "_" <> showT count)
  where
    updateCount (UniqueLocals map) = UniqueLocals (Map.insertWith (+) i 1 map)
    countForIdent (UniqueLocals map) = fromMaybe 0 (Map.lookup i map)

compileDeconstructionAssignment ::
     F.Ident -> T.TypedArgument -> Int -> State UniqueLocals [Expression]
compileDeconstructionAssignment i a n =
  case a of
    T.TAIdentifier t symbol ->
      return
        [ (SetLocal
             (symbolToIdent symbol)
             (forestTypeToWasmType t)
             (Call
                (ident (printWasmType (forestTypeToWasmType t) <> ".load"))
                [Call (ident "i32.add") [GetLocal i, Const $ n * 4]]))
        ]
    T.TANumberLiteral _ -> pure []
    T.TADeconstruction (symbol) _ _ args -> do
      let assignment =
            [ SetLocal
                (symbolToIdent symbol)
                I32
                (Call
                   (ident "i32.load")
                   [Call (ident "i32.add") [GetLocal i, Const $ n * 4]])
            ]
      (assignment <>) <$>
        (concat <$>
         traverse
           (uncurry (compileDeconstructionAssignment (symbolToIdent symbol)))
           (zip args [1 ..]))

compileCaseExpression ::
     Locals
  -> T.TypedExpression
  -> DeclarationCompileState (F.Ident, Expression)
compileCaseExpression locals fexpr =
  let body =
        case fexpr of
          Identifier (T.Applied _ _) i _ ->
            return $ Call (ident "i32.load") [GetLocal (bindingToIdent i)]
          Identifier (T.TL (T.TypeLambda _)) i _ ->
            return $ Call (ident "i32.load") [GetLocal (bindingToIdent  i)]
          _ -> compileExpression locals fexpr
   in do uniqueLocal <- getUniqueLocal (ident "case_result")
         expr <-
           SetLocal uniqueLocal (forestTypeToWasmType (typeOf fexpr)) <$> body
         return (uniqueLocal, expr)

-- TODO - make it more clear that this is only about case arguments
compileArgument ::
     T.TypedExpression
  -> TypedArgument
  -> DeclarationCompileState (Expression, Locals)
compileArgument caseFexpr arg =
  case arg of
    T.TAIdentifier _ i ->
      return (GetLocal (symbolToIdent i), addLocal (symbolToIdent i) noLocals)
    T.TANumberLiteral n -> return (Const n, noLocals)
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
       in return (Block I32 (NE.fromList (assignments <> [Const tag])), locals)
  where
    caseLocal =
      case caseFexpr of
        T.Identifier _ (Binding _ name) _ -> GetLocal (symbolToIdent name)
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
    Sequence exprs ->
      Text.intercalate "\n" $ NE.toList (printWasmExpr <$> exprs)
    Block wasmType exprs ->
      "(block (result " <> printWasmType wasmType <> ")\n" <>
      indent2 (Text.intercalate "\n" $ NE.toList (printWasmExpr <$> exprs)) <>
      "\n)"
    Const n -> "(i32.const " <> showT n <> ")"
    FloatConst n -> "(f32.const " <> showT n <> ")"
    GetLocal name -> "(get_local $" <> F.s name <> ")"
    SetLocal name _ expr' ->
      "(set_local $" <> F.s name <> " " <> printWasmExpr expr' <> ")"
    TeeLocal name _ expr' ->
      "(tee_local $" <> F.s name <> " " <> printWasmExpr expr' <> ")"
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
    locals expr' = (\(a, b) -> (F.s a, b)) <$> findLocals expr'

findLocals :: Expression -> [(F.Ident, WasmType)]
findLocals expr' =
  case expr' of
    TeeLocal name wasmType expr -> [(name, wasmType)] <> findLocals expr
    SetLocal name wasmType expr -> [(name, wasmType)] <> findLocals expr
    Sequence exprs -> concatMap findLocals $ NE.toList exprs
    Block _ exprs -> concatMap findLocals $ NE.toList exprs
    If expr expr' mexpr ->
      findLocals expr <> findLocals expr' <> maybe [] findLocals mexpr
    Call _ exprs -> concatMap findLocals exprs
    NamedCall _ exprs -> concatMap findLocals exprs
    Const _ -> []
    FloatConst _ -> []
    GetLocal _ -> []

printLocal :: (Text, WasmType) -> Text
printLocal (name, wasmType) =
  "(local $" <> name <> " " <> printWasmType wasmType <> ")"

ident :: Text -> F.Ident
ident t = F.Ident $ F.NonEmptyString (Text.head t) (Text.tail t)

symbolToIdent :: T.Symbol -> F.Ident
symbolToIdent (Symbol n i) =
  if F.s i == "main"
    then ident "main"
    else ident (F.s i <> "_" <> showT n)

bindingToIdent :: T.Binding -> F.Ident
bindingToIdent (T.Binding _ symbol) = symbolToIdent symbol
