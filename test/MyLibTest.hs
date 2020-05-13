{-# LANGUAGE OverloadedStrings #-}
-- FIXME: currently this is broken. The only use is that running `cabal test`
-- will at least output to a log that gives accurate results.
module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Paths_filesig
import FileSig


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [testHasSignature, testSignatureMatch, testCount]

testHasSignature :: TestTree
testHasSignature = testCase "hasSignature" $ do

  scriptPath <- getDataFileName "test/fixtures/rtf.rtf"
  scriptBool <- hasSignature scriptPath "rtf"
  assertBool "Failed RTF" scriptBool

  docPath <- getDataFileName "test/fixtures/doc.doc"
  docBool <- hasSignature docPath "doc"
  assertBool "Failed doc" docBool


testSignatureMatch :: TestTree
testSignatureMatch = testCase "signatureMatch" $ do

  rtfPath <- getDataFileName "test/fixtures/rtf.rtf"
  match <- signatureMatch rtfPath
  assertEqual "Failed signatureMatch (rtf)" ["rtf"] match

testCount :: TestTree
testCount = testCase "Count JSON entries" $
  assertEqual "Count entries" 150 (length allExtensions)
