{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}

module WasmSpec
  ( wasmSpecs
  ) where

import Data.Either
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text, unpack)
import qualified Data.Text as T
import System.Exit
import System.IO.Temp
import System.Process
import Test.Hspec
import Test.QuickCheck
import Text.RawString.QQ

import Compiler
import HaskellSyntax
import Language
import TypeChecker
import Wasm

import Arbitrary

instance Testable (IO Bool) where
  property = ioProperty

propCodeThatTypeChecksShouldCompile :: Language.Module -> IO Bool
propCodeThatTypeChecksShouldCompile m =
  case printWasm . forestModuleToWasm <$> checkModule m of
    Right wat -> do
      path <- writeSystemTempFile "wat" (unpack wat)
      exitCode <- system $ "wat2wasm " ++ show path ++ " -o /dev/null"
      case exitCode of
        ExitSuccess -> return True
        ExitFailure _ -> do
          _ <- system "mkdir -p failures"
          writeFile "./failures/last.tree" (unpack wat)
          return False
    Left _ -> return True

wasmSpecs :: SpecWith ()
wasmSpecs =
  parallel $
  describe "wasm code generation" $ do
    it "generates valid wasm for any well typed module" $ do
      withMaxSuccess
        1000
        (property (forAll genModule propCodeThatTypeChecksShouldCompile))
    it "correctly generates functions that return floats" $ do
      let typedModule =
            TypedModule
              [ TypedDeclaration
                  (Ident (NonEmptyString 'g' "etX"))
                  [ TADeconstruction
                      (Ident (NonEmptyString 'P' "layer"))
                      0
                      [TAIdentifier Float' (Ident (NonEmptyString 'x' ""))]
                  ]
                  (Lambda
                     (TL (TypeLambda (Ident (NonEmptyString 'P' "layer"))))
                     Float')
                  (TypeChecker.Identifier Float' (Ident (NonEmptyString 'x' "")))
              ]
       in forestModuleToWasm typedModule `shouldBe`
          Wasm.Module
            [ Func
                (Wasm.Declaration
                   (Ident (NonEmptyString 'g' "etX"))
                   [(Ident (NonEmptyString 'P' "layer"), I32)]
                   F32
                   (Sequence
                      F32
                      (SetLocal
                         (Ident (NonEmptyString 'x' ""))
                         F32
                         (Call
                            (Ident (NonEmptyString 'f' "32.load"))
                            [ Call
                                (Ident (NonEmptyString 'i' "32.add"))
                                [ GetLocal (Ident (NonEmptyString 'P' "layer"))
                                , Const 4
                                ]
                            ]) :|
                       [GetLocal (Ident (NonEmptyString 'x' ""))])))
            ]
            0
