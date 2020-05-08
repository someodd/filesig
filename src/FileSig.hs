-- | Discern a file's type using hex-encoded file signatures.
--
-- Useful: https://en.wikipedia.org/wiki/List_of_file_signatures
module FileSig
    ( hasSignature
    , signatureMatch
    , FileType(..)
    , allFileTypes
    ) where

import Control.Monad.IO.Class
import Data.List
import Data.Ord
import qualified Data.Hex                   as H
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as B8

-- | The hex code (a `String` without spaces) and offset where
-- the magic number/file signature is located, identifying the
-- `FileType`.
type FileSignature = (String, Int)

-- | The various file types/formats which can be detected.
data FileType
  = Script
  -- ^ Like a Bash script
  | GIF89a
  | Exe--TODO: rename; also DLLs have this
  | ZipNonEmpty--TODO: doublecheck
  -- ^ A zip file which actually contains something...
  | PDF
  | Ogg
  | RIFF
  -- ^ Resource Interchange File Format.
  | RIFFWAVE
  -- ^ Waveform Audio File Format (.wav).
  | MP3
  | BMP
  | FLAC
  | MIDI
  | RTF
  -- ^ Rich Text Format
  | XZ
  -- ^ XZ compression utility using LZMA2 compression .
  | SevenZip
  -- ^ 7z archive.
  | Doc
  -- ^ Compound File Binary Format; used by older Microsoft Office documents
  -- like doc, xls, and ppt.
  | JPEGJFIF
  -- ^ JPEG File Interchange Format
  | PNG
  deriving (Show, Enum, Eq)

-- | Contains all the `FileType`s.
allFileTypes = [Script .. PNG]

-- | Get the magic number/`FileSignature` for the supplied `FileType`.
fileSigConst :: FileType -> FileSignature
fileSigConst fileType = case fileType of
  Script      -> ("2321", 0)
  GIF89a      -> ("474946383961", 0)
  Exe         -> ("4D5A", 0)
  ZipNonEmpty -> ("504B0304", 0)
  PDF         -> ("255044462D", 0)
  Ogg         -> ("4F676753", 0)
  RIFF        -> ("52494646", 0)
  RIFFWAVE    -> ("57415645", 0)
  MP3         -> ("494433", 0)
  BMP         -> ("424D", 0)
  FLAC        -> ("664C6143", 0)
  MIDI        -> ("4D546864", 0)
  RTF         -> ("7B5C72746631", 0)
  XZ          -> ("FD377A585A00", 0)
  SevenZip    -> ("377ABCAF271C", 0)
  Doc         -> ("D0CF11E0A1B11AE1", 0)
  JPEGJFIF    -> ("FFD8FFE000104A4649460001", 0)
  PNG         -> ("89504E470D0A1A0A", 0)

-- | Slice a ByteString.
slice :: Int -> Int -> BS.ByteString -> BS.ByteString
slice from to xs = BS.take (to - from + 1) (BS.drop from xs)

-- | Find the longest `FileType` `FileSignature` contained in the file
-- specified by path (if any).
signatureMatch :: String -> [FileType] -> IO (Maybe FileType)
signatureMatch filePath sigsToMatch = do
  let orderedSigsToMatch = sortBy sortBySigLength sigsToMatch
  contents <- BS.readFile filePath
  pure $ find (findFunc contents) orderedSigsToMatch
  where
    findFunc contents ftype = hasSignature' contents ftype
    sortBySigLength a b =
      let sigA = fst $ fileSigConst a
          sigB = fst $ fileSigConst b
      in  sigA `compare` sigB

-- | Test if a `ByteString` matches a supplied `FileType`'s `FileSignature`.
hasSignature' :: BS.ByteString -> FileType -> Bool
hasSignature' contents fileType =
  let (expectedSig, offset) = fileSigConst fileType
      expectedSigBS = B8.pack expectedSig
      potentialSig = slice offset (offset + (BS.length expectedSigBS `div` 2) - 1) contents
  in  H.hex potentialSig == expectedSigBS

-- | Test if the file at the supplied path matches a supplied `FileType`'s `FileSignature`.
hasSignature :: String -> FileType -> IO Bool
hasSignature filePath fileType =
  (`hasSignature'` fileType) <$> BS.readFile filePath
