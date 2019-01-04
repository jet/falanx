# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).
The `Unreleased` section name is replaced by the expected version of next release. A stable versions log contains all changes between that version and the previous stable version (can duplicate the prereleases logs)

<a name="v0.5.0"></a>
## [0.5.0]

<a name="v0.4.0"></a>
## [0.4.0] - 2019-01-04

### Added
- support the `repeated` field rule
- new `Falanx.Templates` template package, with a library example
- allow multiple proto files in the same project
- use `OutputPath` metadata of `ProtoFile` item to specify generated .fs file path
- can be used in standalone console projects, a library is not required

### Changed
- the generated .fs files, by default, are alongside the .proto files, not in `obj` directory. These can be ignored with `*.proto.fs` wildcard, if needed
- the compile files are included at beginning of the compile list as `CompileBefore` instead of `Compile` msbuild item

### Removed
- dependencies not needed at runtime from `Falanx.Proto.Codec.*` packages
- removed the `Falanx.Proto.Core` package, the codecs packages doesnt depend on it anymore

### Fixed
- the .fs file is regenerated on build if the proto file is changed

<a name="v0.4.0-alpha3"></a>
## [0.4.0-alpha3] - 2018-12-28

### Added
- allow multiple proto files in the same project
- use `OutputPath` metadata of `ProtoFile` item to specify generated .fs file path

### Changed
- the generated .fs files, by default, are alongside the .proto files, not in `obj` directory
- the compile files are included at beginning of the compile list (allow use in console app)

### Fixed
- the .fs file is regenerated on build if the proto file is changed

<a name="v0.4.0-alpha2"></a>
## [0.4.0-alpha2] - 2018-12-21

### Added
- support the `repeated` field rule

### Removed
- dependencies not needed at runtime from `Falanx.Proto.Codec.*` packages

<a name="v0.4.0-alpha1"></a>
## [0.4.0-alpha1] - 2018-12-12

### Fixed
- binary serialization of repeated enum ( [#65](https://github.com/jet/falanx/pull/65) by [@wuzzeb](https://github.com/wuzzeb) )

<a name="v0.3.0"></a>
## [0.3.0] - 2018-12-21

### Added
- support for `json` format, use the `Falanx.Proto.Codec.Json` package

### Changed
- renamed `Falanx.BinaryCodec` package to `Falanx.Proto.Codec.Binary`

<a name="v0.2.0"></a>
## [0.2.0] - 2018-10-3

### Added
- the `Deserialization` helper method
- the `Falanx.Sdk` package to integrate with .NET Sdk projects (fsproj)

[0.5.0]: https://github.com/jet/falanx/compare/v0.4.0...HEAD
[0.4.0]: https://github.com/jet/falanx/compare/v0.3.0...v0.4.0
[0.4.0-alpha3]: https://github.com/jet/falanx/compare/v0.4.0-alpha2...v0.4.0-alpha3
[0.4.0-alpha2]: https://github.com/jet/falanx/compare/v0.4.0-alpha1...v0.4.0-alpha2
[0.4.0-alpha1]: https://github.com/jet/falanx/compare/v0.3.0...v0.4.0-alpha1
[0.3.0]: https://github.com/jet/falanx/compare/v0.2.0...v0.3.0
[0.2.0]: https://github.com/jet/falanx/compare/22743b53ac81e4f91df68cd9fbdea7086d88e746...v0.2.0
