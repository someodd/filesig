-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Discern a file's type using hex-encoded file signatures.
--
-- Useful: https://en.wikipedia.org/wiki/List_of_file_signatures

module FileSig
  ( hasSignature
  , signatureMatch
  , MagicMap
  , magicMap
  , allExtensions
  , prettyMatchPrint
  )  where

import Paths_filesig
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Base16 (decodeBase16Lenient)
import Data.HashMap.Strict as HM
import Data.List as L (sort)
import Data.Ord (Down(..))
import Data.Text as T (Text, pack, unlines, unpack)
import System.IO (IOMode(..), hClose, withBinaryFile)


import Magic


-- | Magic bytes/file signature, in the form of the offset (`Int`), followed
-- by the bytes (`ByteString`).
type Signature = (Int, ByteString)

-- | The extension of a file format, which acts as a key in the magic database.
type ExtensionKey = Text

-- | The MIME type/media type
type MIME = Text

-- | File format located by a file extension (as key) in the magic number database (JSON).
data FileFormat = FileFormat {
  magicBytes :: [Signature],
  -- ^ The magic numbers/file signatures which can be used to identify this file format.
  mime :: MIME
  -- ^ the MIME type/media type of this file format.
} deriving (Show)

-- | The abstraction of the JSON magic database containing all the file signatures.
-- The key is the file extension.
type MagicMap = HM.HashMap ExtensionKey FileFormat

-- | This is the entire MagicMap to be used everywhere, no IO required.
--
-- The 'decodeBase16Lenient' interprets the hexidecimal into raw bytestring,
-- So it is already in optimal form at runtime.
magicMap :: MagicMap
magicMap =
    HM.map go rawMagicMap
  where
    go :: ([Signature], Text) -> FileFormat
    go (sigs, mime) = FileFormat (fmap decodeBase16Lenient <$> sigs) mime

-- | All extensions in the 'magicMap'
allExtensions :: [ExtensionKey]
allExtensions = L.sort $ HM.keys magicMap

-- This is going to be '(36865, "4344303031")' from the 'iso' signature
-- but if the magicMap is ever updated, this will consistently give the
-- biggest starting chunk we might need to guess extentions, so as to not
-- load in potential GBs of data into memory.
longestSignature :: Int
longestSignature = longestChunk allSigs
  where
    allSigs = concatMap magicBytes $ HM.elems magicMap

sigLength :: Signature -> Int
sigLength (i, bs) = i + BS.length bs

getLongestSignature :: ExtensionKey -> Maybe Int
getLongestSignature ext =
    longestChunk . magicBytes <$> mFileExt
  where
    mFileExt = HM.lookup ext magicMap

longestChunk :: [Signature] -> Int
longestChunk [] = 0
longestChunk sigs = maximum $ sigLength <$> sigs

-- | Is the contents the file type defined (by the supplied extension)? Nothing
-- if the file extension supplied doesn't exist in the `HashMap`.
hasSignature' :: ExtensionKey -> BSL.ByteString -> Bool
hasSignature' ext contents =
    -- If the ext doesn't exist, we can't verify it,
    -- so we default to 'False'.
    maybe False anyValidSig $ HM.lookup ext magicMap
  where
    anyValidSig :: FileFormat -> Bool
    anyValidSig = or . fmap checkOneSig . magicBytes
    checkOneSig :: Signature -> Bool
    checkOneSig (offset, expectedSig) =
      let lazyExpectedSig = BSL.fromStrict expectedSig
          len = BSL.length lazyExpectedSig
          potentialSig = BSL.take len $ BSL.drop (fromIntegral offset) contents
      in potentialSig == lazyExpectedSig

-- | Check if the file at the suppiled `FilePath` matches a file format from the database.
-- `Nothing` if no such format (specified by extension) exists in the database.
hasSignature :: ExtensionKey -> FilePath -> IO Bool
hasSignature ext filePath =
    case ext `HM.lookup` magicMap of
      Nothing -> return False
      Just (FileFormat sigs _) ->
        -- Doing this before checking makes sure we only read in the
        -- minimum amount of bytes needed to check for this extension.
        let maxExtBytes = longestChunk sigs
        in hasSignature' ext <$> getFirstNBytes filePath maxExtBytes

-- | Get the first n bytes of the file at `FilePath`, possibly less if
-- the file is smaller than n.
getFirstNBytes :: FilePath -> Int -> IO BSL.ByteString
getFirstNBytes fp = withBinaryFile fp ReadMode . flip BSL.hGet

-- Some optimizations possible with 'signatureMatch':
-- * Pass length of 'contents' to hasSignature', so it can dismiss some ext flat-out
-- * Do longest checks first, so the 'contents' can be shortened and
--   memory can be freed up for the following checks.

-- FIXME: collect all that match and then return the longest one?
-- | Check if the supplied `FilePath` matches any of the file formats in the magic database.
signatureMatch :: FilePath -> IO [ExtensionKey]
signatureMatch filePath = do
  contents <- getFirstNBytes filePath longestSignature
  -- FIXME/TODO: sort by the longest signature
  pure $ Prelude.filter (flip hasSignature' contents) allExtensions

prettyMatchPrint :: [ExtensionKey] -> IO ()
prettyMatchPrint = mapM_ prettyOneMatch
  where
    putTextLn = putStrLn . T.unpack
    prettyOneMatch ext =
      case HM.lookup ext magicMap of
        Just (FileFormat sigs mime) ->
          let typicalExt = "Typical Extension: ." <> ext
              mediaType  = "MIME/Media Type:   " <> mime
              signatures = "Signatures:        " <> T.pack (show sigs)
          in putTextLn $ T.unlines [typicalExt, mediaType, signatures] <> "\n"
        Nothing -> putTextLn $ "No support for this extension: ." <> ext
