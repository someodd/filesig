{-# LANGUAGE OverloadedStrings #-}
-- | Discern a file's type using hex-encoded file signatures.
--
-- Useful: https://en.wikipedia.org/wiki/List_of_file_signatures

module FileSig
  ( hasSignature
  , signatureMatch
  , getAllExtensions
  , decodedJSON
  )  where

import Paths_filesig
import Data.Vector as V
import Data.Maybe
import Data.Functor
import Data.Traversable
import Control.Monad
import Data.Text as T
import Data.HashMap.Strict as HM
import Data.Aeson
import Data.Aeson.Types
import Control.Monad.IO.Class
import Data.List                            as L
import Data.Ord
import qualified Data.Hex                   as H
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy as B

type Signature = (String, Int)

magicFileJSON :: FilePath
magicFileJSON = "data/magic.json"

getJSON :: IO B.ByteString
getJSON = do
  docPath <- getDataFileName magicFileJSON
  B.readFile docPath

decodedJSON = do
  theJSON <- getJSON
  let (Right daJSON) = eitherDecode' theJSON :: Either String (HM.HashMap Text Value)
  pure daJSON

-- | Slice a ByteString.
byteSlice :: Int -> Int -> BS.ByteString -> BS.ByteString
byteSlice from to xs = BS.take (to - from + 1) (BS.drop from xs)

getSignatures :: Value -> [Signature]
getSignatures (Object fileTypeMap) =
  let signs = HM.lookup (pack "signs") fileTypeMap-- FIXME:fromjust
      unparsedSignatures = case signs of
        (Just signatures) -> listOfText signatures
        Nothing           -> error "Malformed magic JSON: entry without signs entry." -- TODO: could tell key
  in  L.map parseSig (unparsedSignatures)
  where
    listOfText (Array a) =
      let blah (String x) = x
          aList = V.toList a
      in  L.map blah aList

    parseSig :: Text -> (String, Int)
    parseSig sign =
      let [offset, hex] = T.splitOn "," sign
      in  (T.unpack hex, read (T.unpack offset) :: Int)
getSignatures _ = mzero

-- | Is the contents the file type defined (by the supplied extension)? Nothing
-- if the file extension supplied doesn't exist in the `HashMap`.
hasSignature' :: HM.HashMap Text Value -> BS.ByteString -> Text -> Maybe Bool
hasSignature' magicMap contents fileExtension =
  let extEntry = HM.lookup fileExtension magicMap
  in  case extEntry of
    (Just signatures) -> Just $ L.or $ L.map checkOneSig (getSignatures $ signatures)
    Nothing           -> Nothing 
  where
    checkOneSig sig =
      let (expectedSig, offset) = sig
          expectedSigBS = B8.pack expectedSig
          potentialSig = byteSlice offset (offset + (BS.length expectedSigBS `div` 2) - 1) contents
      in  H.hex potentialSig == expectedSigBS

hasSignature :: HM.HashMap Text Value -> Text -> Text -> IO (Maybe Bool)
hasSignature magicMap filePath fileExtension = do
  daFile <- BS.readFile (T.unpack filePath)
  pure $ hasSignature' magicMap daFile fileExtension

getAllExtensions :: HM.HashMap Text Value -> [Text]
getAllExtensions magicMap = HM.keys magicMap

-- FIXME: pass optional list of exts?
-- | Find the longest `FileType` `FileSignature` contained in the file
-- specified by path (if any).
signatureMatch magicMap filePath = do
  contents <- BS.readFile (T.unpack filePath)
  -- FIXME/TODO: sort by the longest signature
  pure $ L.find (\x -> fromMaybe False (hasSignature' magicMap contents x)) (getAllExtensions magicMap)
