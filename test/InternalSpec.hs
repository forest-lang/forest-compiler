
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}
module InternalSpec (internalSpecs) where

import System.Exit
import System.IO.Temp
import System.Process
import Test.Hspec

import Internal

internalSpecs :: SpecWith ()
internalSpecs =
  describe "Type checker" $ do
    it "checks valid expressions" $
      let
        expr = Call "add" [Literal (Number 1), Literal (Number 1)]
      in
        check expr `shouldBe` Right ()
    it "checks invalid expressions" $
      let
        expr = Call "add" [Literal (Number 1), Literal (String "test")]
      in
        check expr `shouldBe` Left (CompileError "No function add for Number -> String")
    it "gives useful messages" $
      let
        expr = Call "add" [Literal (String "test"), Literal (Number 1)]
      in
        check expr `shouldBe` Left (CompileError "No function add for String -> Number")

