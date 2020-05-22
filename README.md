# FileSig: Detect Filetypes Using Only Magic Numbers in Pure Haskell

Pure Haskell library to determine a file's type without relying on its
name/extension (by using its file signature (AKA "magic number").

Thanks to @qti3e for supplying the initial database in JSON from [this Gist
list of file
signatures](https://gist.github.com/qti3e/6341245314bf3513abb080677cd1c93b).

It does not rely on file extensions at all.

For a full list of file types supported (>=150!) in the database, please see
`data/magic.json`.

## Examples

See if a file has a signature by supplying a file path and an extension. Note that
it doesn't use the extension to check, the extension is merely used to specify the
file type magic number to use.

```haskell
>>> hasSignature "foo/myrichtext" "rtf"
True
```

To check if, wand which, file type a file matches in the database:

```haskell
>>> signatureMatch "foo/bar/story"
Just "doc"
```

## Running tests

`cabal test`
