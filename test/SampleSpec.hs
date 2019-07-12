{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}

module SampleSpec
  ( validSampleSpecs, invalidSampleSpecs
  ) where

import Data.Either
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory
import System.Exit
import System.IO.Temp
import System.Process
import Test.Hspec
import Text.RawString.QQ

import Compiler
import HaskellSyntax
import Language
import TypeChecker

validSampleSpecs :: IO (SpecWith ())
validSampleSpecs = do
  files <-
    filter (not . isPrefixOf ".") <$>
    getDirectoryContents "./test/samples/valid"
  specs <- foldl1 (flip (>>)) <$> mapM testFileIsValid files
  return $ parallel $ describe "valid samples" specs
  where
    testFileIsValid :: FilePath -> IO (SpecWith ())
    testFileIsValid path = do
      contents <- TIO.readFile $ "./test/samples/valid/" <> path
      return $
        it (path <> " is valid") $
          case check contents of
            Success _ -> True
            ParseErr _ -> False
            CompileErr _ -> False

invalidSampleSpecs :: IO (SpecWith ())
invalidSampleSpecs = do
  files <-
    filter (not . isPrefixOf ".") <$>
    getDirectoryContents "./test/samples/invalid"
  specs <- foldl1 (flip (>>)) <$> mapM testFileIsValid files
  return $ describe "invalid samples" specs
  where
    testFileIsValid :: FilePath -> IO (SpecWith ())
    testFileIsValid path = do
      contents <- TIO.readFile $ "./test/samples/invalid/" <> path
      return $
        it (path <> " is not valid") $
          case check contents of
            Success _ -> False
            ParseErr _ -> True
            CompileErr _ -> True

