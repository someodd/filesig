{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Discern a file's type using hex-encoded file signatures.
--
-- Useful: https://en.wikipedia.org/wiki/List_of_file_signatures

module FileSig
  ( hasSignature
  , signatureMatch
  , getAllExtensions
  , decodedJSON
  , prettyMatchPrint
  )  where

import Data.Foldable
import Data.FileEmbed
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

-- | Magic bytes/file signature, in the form of the hex (`String` [no spaces]), followed
-- by the offset (`Int`).
type Signature = (String, Int)

-- | The extension of a file format, which acts as a key in the magic database.
type ExtensionKey = Text

-- | File format located by a file extension (as key) in the magic number database (JSON).
data FileFormat = FileFormat {
  magicBytes :: [Signature],
  -- ^ The magic numbers/file signatures which can be used to identify this file format.
  mime :: Text
  -- ^ the MIME type/media type of this file format.
} deriving (Show)

-- | The abstraction of the JSON magic database containing all the file signatures.
-- The key is the file extension.
type MagicMap = HM.HashMap ExtensionKey FileFormat

-- | JSON parser for a file format entries in the magic database.
instance FromJSON FileFormat where
  parseJSON = withObject "FileFormat" $ \o ->
    FileFormat <$> fmap parseSignatures (o .: "signs")
            <*> o .: "mime"

-- | Parse the JSON "signs" entry from the magic database.
parseSignatures :: Value -> [Signature]
parseSignatures someTexts@(Array _) = L.map parseSig (listOfText someTexts)
  where
    listOfText (Array a) =
      let blah (String x) = x
          aList = V.toList a
      in  L.map blah aList

    parseSig :: Text -> Signature
    parseSig sign =
      let [offset, hex] = T.splitOn "," sign
      in  (T.unpack hex, read (T.unpack offset) :: Int)
parseSignatures _ = mzero

getJSON :: B.ByteString
getJSON = do
  B.fromStrict $(embedFile "data/magic.json")

decodedJSON :: IO MagicMap
decodedJSON = do
  let (Right daJSON) = eitherDecode' getJSON :: Either String MagicMap
  pure daJSON

-- | Slice a ByteString.
byteSlice :: Int -> Int -> BS.ByteString -> BS.ByteString
byteSlice from to xs = BS.take (to - from + 1) (BS.drop from xs)

-- | Is the contents the file type defined (by the supplied extension)? Nothing
-- if the file extension supplied doesn't exist in the `HashMap`.
hasSignature' :: MagicMap -> BS.ByteString -> Text -> Maybe Bool
hasSignature' magicMap contents fileExtension =
  let extEntry = HM.lookup fileExtension magicMap
  in  case extEntry of
    (Just signatures) -> Just $ L.or $ L.map checkOneSig (magicBytes signatures) -- FIXME: Just fileExt
    Nothing           -> Nothing 
  where
    checkOneSig sig =
      let (expectedSig, offset) = sig
          expectedSigBS = B8.pack expectedSig
          potentialSig = byteSlice offset (offset + (BS.length expectedSigBS `div` 2) - 1) contents
      in  H.hex potentialSig == expectedSigBS

-- | Check if the file at the suppiled `FilePath` matches a file format from the database.
-- `Nothing` if no such format (specified by extension) exists in the database.
hasSignature :: MagicMap -> FilePath -> Text -> IO (Maybe Bool)
hasSignature magicMap filePath fileExtension = do
  daFile <- BS.readFile filePath
  pure $ hasSignature' magicMap daFile fileExtension

getAllExtensions :: MagicMap -> [Text]
getAllExtensions magicMap = HM.keys magicMap

-- FIXME: collect all that match and then reutrn the longest one?
-- | Check if the supplied `FilePath` matches any of the file formats in the magic database.
signatureMatch :: HM.HashMap Text FileFormat -> FilePath -> IO [ExtensionKey]
signatureMatch magicMap filePath = do
  contents <- BS.readFile filePath
  -- FIXME/TODO: sort by the longest signature
  pure $ L.filter (\x -> fromMaybe False (hasSignature' magicMap contents x)) (getAllExtensions magicMap)

prettyMatchPrint :: MagicMap -> [ExtensionKey] -> IO ()
prettyMatchPrint magicMap exts = traverse_ prettyOneMatch exts
  where
  prettyOneMatch ext =
    let entry = HM.lookup ext magicMap
    in  case entry of
      (Just fileFormat) ->
        let typicalExt = "Typical Extension: ." L.++ (T.unpack ext)
            mediaType  = "MIME/Media Type:   " L.++ (T.unpack $ mime fileFormat)
            signatures = "Signatures:        " L.++ (show $ magicBytes fileFormat)
        in  putStrLn $ typicalExt L.++ "\n" L.++ mediaType L.++ "\n" L.++ signatures L.++ "\n"
      Nothing  -> error "This shouldn't be possible!"
