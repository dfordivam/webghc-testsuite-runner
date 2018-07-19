{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main where

import Prelude hiding (FilePath)
import           Shelly
import qualified Data.Text as T
import           Filesystem.Path ( replaceExtension, basename, directory, extension, addExtension
                                ,hasExtension , filename, addExtensions, dropExtensions)

import           Test.HUnit.Base (assertBool, assertFailure, assertEqual, Assertion)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.Framework
import           Data.Char (isLower, toLower, isDigit, isSpace)
import           Filesystem.Path.CurrentOS (fromText, toText, encodeString)
import           Data.Maybe

default (T.Text)

main = shelly . silently $ doGhcTests

doGhcTests = do
  testRoot <- pwd
  hsSrcs <- findWhen isHsLhs testRoot

  -- liftIO $ print hsSrcs
  -- mapM doOneTest hsSrcs
  t <- doOneTest (head hsSrcs)
  liftIO $ defaultMain [t]



isHsLhs fp = return $ isTestFile fp
  where
    testFirstChar c = isLower c || isDigit c
    isTestFile file =
      (extension file == Just "hs" || extension file == Just "lhs") &&
      ((maybe False testFirstChar . listToMaybe . encodeString . basename $ file) ||
      (basename file == "Main"))

doOneTest :: FilePath -> Sh Test
doOneTest fp = do
  o1 <- compileRunGhc fp

  o2 <- compileRunWasm fp

  return $ testCase "" (assertEqual "" o1 o2)

compileRunGhc fp = do
  let exe = replaceExtension fp "ghc"
      out = replaceExtension fp "ghc_out"
  run_ "ghc" [toTextIgnore fp, "-o", toTextIgnore exe]
  run exe []


compileRunWasm fp = do
  let exe = replaceExtension fp "wasm"
      out = replaceExtension fp "wasm_out"
  run_ "wasm32-unknown-unknown-wasm-ghc" [toTextIgnore fp, "-o", toTextIgnore exe]
  cd "/home/divam/repos/wasm/webabi/"
  run "node" [ "run_node.js", toTextIgnore exe]
