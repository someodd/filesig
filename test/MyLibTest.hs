{-# LANGUAGE OverloadedStrings #-}
-- FIXME: currently this is broken. The only use is that running `cabal test`
-- will at least output to a log that gives accurate results.
module Main (main) where

import System.Directory (getCurrentDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import Paths_filesig
import FileSig


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testHasSignature
  , testSignatureMatch
  , testCount
  ]

getFixturesPath :: IO FilePath
getFixturesPath = (++ "/test/fixtures") <$> getCurrentDirectory

testHasSignature :: TestTree
testHasSignature = testCase "hasSignature" $ do

  path <- getFixturesPath
  -- scriptPath <- getDataFileName "test/fixtures/rtf.rtf"

  scriptBool <- hasSignature "rtf" $ path ++ "/rtf.rtf"
  assertBool "Failed RTF" scriptBool

  -- docPath <- getDataFileName "test/fixtures/doc.doc"
  docBool <- hasSignature "doc" $ path ++ "/doc.doc"
  assertBool "Failed doc" docBool


testSignatureMatch :: TestTree
testSignatureMatch = testCase "signatureMatch" $ do

  path <- getFixturesPath
  -- rtfPath <- getDataFileName "test/fixtures/rtf.rtf"
  match <- signatureMatch $ path ++ "/rtf.rtf"
  assertEqual "Failed signatureMatch (rtf)" ["rtf"] match

testCount :: TestTree
testCount = testCase "Count extension entries" $
  assertEqual "Count entries" 150 (length allExtensions)
