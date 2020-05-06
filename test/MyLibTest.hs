-- FIXME: currently this is broken. The only use is that running `cabal test`
-- will at least output to a log that gives accurate results.
module Main (main) where

import Test.HUnit

import FileSig

testHasSignature = TestCase (do
  hasSignature "test/fixtures/script.sh" Script >>= \x -> assertBool "Failed Script" x
  hasSignature "test/fixtures/rtf.rtf" RTF >>= \x -> assertBool "Failed RTF" x
  hasSignature "test/fixtures/doc.doc" Doc >>= \x -> assertBool "Failed Doc" x
  )

testSignatureMatch = TestCase (do
  let sigsToMatch = [ Script
                    , Doc
                    , FLAC
                    , SevenZip
                    , JPEGJFIF
                    ]
  theMatch <- signatureMatch "test/fixtures/doc.doc" allFileTypes
  assertEqual "Failed signatureMatch" theMatch (Just Doc)
  )

tests = TestList
  [ TestLabel "testHasSignature Tests" testHasSignature
  , TestLabel "testSignatureMatch Tests" testSignatureMatch
  ]

main :: IO ()
main = runTestTT tests >> pure ()
