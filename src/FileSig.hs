-- {-# LANGUAGE TemplateHaskell #-}
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

-- import Control.Monad.IO.Class
-- import Data.Aeson
-- import Data.Aeson.Types
-- import Data.FileEmbed
-- import qualified Data.ByteString.Lazy as B
-- import Data.Vector as V
import Paths_filesig
import qualified Data.ByteString            as BS
import Data.ByteString.Base16 (decodeBase16Lenient)
import Data.HashMap.Strict as HM
import Data.Ord (Down(..))
import Data.Text as T

-- | Magic bytes/file signature, in the form of the offset (`Int`), followed
-- by the bytes (`ByteString`).
type Signature = (Int, ByteString)

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
{-
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
-}

-- | This is the entire MagicMap to be used everywhere, no IO required.
--
-- The 'decodeBase16Lenient' interprets the hexidecimal into raw bytestring,
-- So it is already in optimal form at runtime.
magicMap :: MagicMap
magicMap =
    HM.map go rawMagicMap
  where
    go (sigs, mime) = (decodeBase16Lenient <$> sigs, mime)

allExtensions :: [ExtensionKey]
allExtensions = HM.keys magicMap

longestSignature :: ByteString
longestSignature = maximumBy (compare `on` BS.length) allSigs
  where
    allSigs = concatMap (fmap snd . fst) $ HM.elems magicMap

-- | Is the contents the file type defined (by the supplied extension)? Nothing
-- if the file extension supplied doesn't exist in the `HashMap`.
hasSignature' :: Text -> BS.ByteString -> Bool
hasSignature' fileExtension contents =
    -- If the ext doesn't exist, we can't verify it,
    -- so we default to 'False'.
    maybe False (anyValidSig . magicBytes) mFileExt
  where
    anyValidSig = or . fmap checkOneSig
    mFileExt = HM.lookup fileExtension magicMap
    checkOneSig (offset, expectedSig) =
      let potentialSig = BS.take (BS.length expectedSig) $ BS.drop offset contents
      in  potentialSig == expectedSig

-- | Check if the file at the suppiled `FilePath` matches a file format from the database.
-- `Nothing` if no such format (specified by extension) exists in the database.
hasSignature :: Text -> FilePath -> IO Bool
hasSignature fileExtension =
    fmap (hasSignature' fileExtension) . efficientRead
  where
    -- TODO: This should use something that
    -- doesn't read the entire file into memory
    efficientRead = BS.readFile

{-
getAllExtensions :: MagicMap -> [Text]
getAllExtensions magicMap = HM.keys magicMap
-}

-- FIXME: collect all that match and then reutrn the longest one?
-- | Check if the supplied `FilePath` matches any of the file formats in the magic database.
signatureMatch :: FilePath -> IO [ExtensionKey]
signatureMatch filePath = do
  contents <- BS.readFile filePath
  -- FIXME/TODO: sort by the longest signature
  pure $ filter (flip hasSignature' contents) allExtensions

prettyMatchPrint :: [ExtensionKey] -> IO ()
prettyMatchPrint = traverse_ prettyOneMatch
  where
    prettyOneMatch ext =
      case HM.lookup ext magicMap of
        Just (FileFormat sigs mime) ->
          let typicalExt = "Typical Extension: ." <> ext
              mediaType  = "MIME/Media Type:   " <> mime
              signatures = "Signatures:        " <> T.pack (show sigs)
              putTextLn = putStrLn . T.unpack
          in  putTextLn $ T.unlines [typicalExt, mediaType, signatures] <> "\n"
        Nothing -> putTextLn $ "No support for this extension: ." <> ext
