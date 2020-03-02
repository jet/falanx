[![Build Status](https://dev.azure.com/jet-opensource/opensource/_apis/build/status/jet.falanx?branchName=master)](https://dev.azure.com/jet-opensource/opensource/_build/latest?definitionId=8?branchName=master)
[![NuGet](https://img.shields.io/nuget/v/Falanx.Tool.svg)](https://www.nuget.org/packages/Falanx.Tool/)

# Falanx code generation

This repository contains the code generator to generate F# source (.fs files) from Protobuf v3 schema.

The general concepts are as follows:

* Code generation, to generate F# source code, rather than types being injected as a type provider.
* Idiomatic F# code is generated rather than simple .NET 1.1 era code. This means records, discriminated unions, etc., are generated where appropriate.

## How to use

In a .NET Sdk library project, add the following packages

```xml
<PackageReference Include="Falanx.Proto.Codec.Binary" Version="0.5.*" />
<PackageReference Include="Falanx.Proto.Codec.Json" Version="0.5.*" />
<PackageReference Include="Falanx.Sdk" Version="0.5.*" PrivateAssets="All" />
```

It's possibile to use only one of `Falanx.Proto.Codec.Binary` and `Falanx.Proto.Codec.Json` or both, the generated code will depends on the packages referenced

Now specify the `.proto` file path like

```xml
  <ItemGroup>
    <ProtoFile Include="..\proto\bundle.proto" />
  </ItemGroup>
```

and an auto generated file will be created on build

More info in [example-sdk/README.md](example-sdk/README.md)

### Template

Install the .NET template for an example library sample

```
dotnet new -i Falanx.Templates
dotnet new falanx
```

use `--codec` argument to specify the codecs (values `json`,`binary`,`all`)

### Tool

It's possibile to use falanx as command line .NET global tool

```
dotnet tool install -g Falanx.Tool
falanx --help
```

To generate a `.fs` file for a specified `.proto` file:

```
falanx --inputfile test\examples\schemas\bundle.proto --defaultnamespace test --outputfile bundle.fs
```

More info in [example\README.md](example/README.md)

## How to build Falanx

More info in [docs/developer_guide.md](docs/developer_guide.md)

Use the `src/Falanx.sln` solution for development, or directly the projects with .NET Core Sdk (`dotnet`).

Projects:

- `src/Falanx.Tool` the falanx console app, run with `dotnet run`
- `test/Falanx.Tests` the unit test, run with `dotnet run`
- `test/Falanx.IntegrationTests` the integration tests, run with `dotnet run`

As shortcuts, from root:

- `dotnet build` to build the falanx executable `Falanx.Tool`.
- `dotnet pack` to generate packages in `bin/nupks`
- `dotnet test -v n` to run tests

### To build packages

From root

```
dotnet pack
```

The nupkgs will be in `bin/nupkg`

To specify a version pass the `Version` property like `/p:Version=0.1.0-alpha7`

### Info

Falanx uses:

- `Type Provider SDK` common type for quotation and AST support
- `FsAst` untyped F# AST to code via the code formatter Fantomas
- `Froto` protobuf parser and binary serializer
- `Fantomas` code formatter and linter

## Security
This repository is actively monitored by Jet Engineers and the Jet Security team. Please monitor this repo for security updates and advisories. For more information and contacts, please see [SECURITY](security.md)



