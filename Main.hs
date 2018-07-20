{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
-- import qualified Control.Exception as C
import qualified Control.Monad.Catch as C

default (T.Text)

main = shelly . silently $ doGhcTests

doGhcTests = do
  testRoot <- pwd
  hsSrcs <- findWhen isHsLhs testRoot

  -- liftIO $ print hsSrcs
  -- mapM doOneTest hsSrcs
  -- t <- doOneTest (head hsSrcs)
  liftIO $ defaultMain $ map (buildTest . doOneTest) hsSrcs

isHsLhs fp = return $ isTestFile fp
  where
    testFirstChar c = isLower c || isDigit c
    isTestFile file =
      (extension file == Just "hs" || extension file == Just "lhs") &&
      ((maybe False testFirstChar . listToMaybe . encodeString . basename $ file) ||
      (basename file == "Main"))

doOneTest :: FilePath -> IO Test
doOneTest fp = shelly . verbosely $ do
  o2 <- compileRunWasm fp
  o1 <- compileRunGhc fp

  return $ testCase "" (assertEqual "" o1 o2)

compileRunGhc fp = do
  cd (directory fp)
  let exe = replaceExtension fp "ghc"
      out = replaceExtension fp "ghc_out"
      comp = do
        run_ "ghc" [toTextIgnore fp, "-o", toTextIgnore exe]
        return (Right ())
  v <- comp `C.catch`  \(e :: C.SomeException) -> return (Left ())
  case v of
    (Right _) -> do
      o <- run exe []
      return (Right o)
    (Left ()) -> return $ Left "Compilation error"



compileRunWasm fp = do
  cd (directory fp)
  let exe = replaceExtension fp "wasm"
      out = replaceExtension fp "wasm_out"
      comp = do
        run_ "wasm32-unknown-unknown-wasm-ghc" [toTextIgnore fp, "-o", toTextIgnore exe]
        return (Right ())
  v <- comp `C.catch`  \(e :: C.SomeException) -> return (Left ())
  case v of
    (Right _) -> do
      cd "/home/divam/repos/wasm/webabi/"
      o <- run "node" [ "run_node.js", toTextIgnore exe]
      return (Right o)
    (Left ()) -> return $ Left "Compilation error"
