# Revision history for filesig

## v0.1.0.0-rc.3 -- 2020-05-13

### Added

* Standlone executable for using filesig as a CLI app
* Pretty printing function for match results

### Changed

* JSON magic database now gets embedded with file-embed library
* `signatureMatch` now returns a list of all signatures matched

### Fixed

* An error in the ZIP entry in the magic database (comma shouldn't have been in
  the offset!)

## v0.1.0.0-rc.2 -- 2020-05-13

### Added

* Abstractions for the magic database
* JSON parser to parse JSON into the new abstractions
* Better documentation

### Changed

* Update code to use the new magic database abstractions
* File paths are now `FilePath`s instead of `Text`
* Order of `assertEqual` arguments, because what was expected and the value to
  be checked was backwards before.

## v0.1.0.0-rc.1 -- 2020-05-12

* First version. Released on an unsuspecting world.
