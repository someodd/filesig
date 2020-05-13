{-# LANGUAGE OverloadedStrings #-}
-- FIXME: currently this is broken. The only use is that running `cabal test`
-- will at least output to a log that gives accurate results.
module Main (main) where

import Data.Maybe
import qualified Data.Text as T

import Test.Tasty
import Test.Tasty.HUnit

import Paths_filesig
import FileSig

testHasSignature :: TestTree
testHasSignature = testCase "hasSignature" $ do
  magic <- decodedJSON

  scriptPath <- getDataFileName "test/fixtures/rtf.rtf"
  scriptBool <- hasSignature (magic) scriptPath "rtf"
  assertBool "Failed RTF" (isJust scriptBool && fromJust scriptBool)

  docPath <- getDataFileName "test/fixtures/doc.doc"
  docBool <- hasSignature (magic) docPath "doc"
  assertBool "Failed doc" (isJust docBool && fromJust docBool)


testSignatureMatch :: TestTree
testSignatureMatch = testCase "signatureMatch" $ do
  magic <- decodedJSON

  rtfPath <- getDataFileName "test/fixtures/rtf.rtf"
  match <- signatureMatch magic rtfPath
  assertEqual "Failed signatureMatch (rtf)" (Just "rtf") match

testCount :: TestTree
testCount = testCase "Count JSON entries" $ do
  magic <- decodedJSON
  assertEqual "Count entries" 150 (length $ getAllExtensions magic)

tests :: TestTree
tests = testGroup "Tests" [testHasSignature, testSignatureMatch, testCount]

main = defaultMain tests
