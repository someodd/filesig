-- FIXME: currently this is broken. The only use is that running `cabal test`
-- will at least output to a log that gives accurate results.
module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Paths_filesig
import FileSig

testHasSignature :: TestTree
testHasSignature = testCase "hasSignature" $ do
  scriptPath <- getDataFileName "test/fixtures/script.sh"
  scriptPath `hasSignature` Script >>= \x -> assertBool "Failed Script" x

  rtfPath <- getDataFileName "test/fixtures/rtf.rtf"
  rtfPath `hasSignature` RTF >>= \x -> assertBool "Failed RTF" x

  docPath <- getDataFileName "test/fixtures/doc.doc"
  docPath `hasSignature` Doc >>= \x -> assertBool "Failed Doc" x

testSignatureMatch :: TestTree
testSignatureMatch = testCase "signatureMatch" $ do
  let sigsToMatch = [ Script
                    , Doc
                    , FLAC
                    , SevenZip
                    , JPEGJFIF
                    ]
  docPath <- getDataFileName "test/fixtures/doc.doc"
  theMatch <- signatureMatch docPath allFileTypes
  assertEqual "Failed signatureMatch" theMatch (Just Doc)

tests :: TestTree
tests = testGroup "Tests" [testHasSignature, testSignatureMatch]

main = defaultMain tests
