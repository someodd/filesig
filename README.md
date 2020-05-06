# FileSig: Haskell Library

This is a library allowing you to determine a file's type using its file
signature (hex). It does not rely on file extensions at all.

## Examples

```haskell
>>> signatureMatch "path/to/file.midi" allFileTypes
Just MIDI

>>> signatureMatch "path/to/file.doc" [PDF, Exe, FLAC]
Nothing

>>> hasSignature "path/to/file.zip" ZipNonEmpty
True
```
