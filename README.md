[![Build Status](https://dev.azure.com/jet-opensource/opensource/_apis/build/status/jet.falanx?branchName=master)](https://dev.azure.com/jet-opensource/opensource/_build/latest?definitionId=8?branchName=master)
[![NuGet](https://img.shields.io/nuget/v/Falanx.Tool.svg)](https://www.nuget.org/packages/Falanx.Tool/)

# Falanx code generation

This repository contains the code generator to generate F# source (.fs files) from `proto3` source.

The general concepts are as follows:

* Reuse as much off the shelf code as possible for the mvp as to get a feel for how things will work and refine from there.
* Code generation, to generate F# source code, rather than types being injected as a type provider.
* Idiomatic F# code is generated rather than simple .NET 1.1 era code. This means records, discriminated unions, etc., are generated where appropriate.

# How to use

In a .NET Sdk library project, add the following packages

```xml
<PackageReference Include="Falanx.Proto.Codec.Binary" Version="0.4.0" />
<PackageReference Include="Falanx.Proto.Codec.Json" Version="0.4.0" />
<PackageReference Include="Falanx.Sdk" Version="0.4.0" PrivateAssets="All" />
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

## Template

A .NET template exists

```
dotnet new -i Falanx.Templates
dotnet new falanx
```

use `--codec` argument to specify the codecs (values `json`,`binary`,`all`)

## Tool

It's also possibile to use falanx as command line .net global tool, see [example\README.md](example/README.md)

```
dotnet tool install -g Falanx.Tool
falanx --help
```

# Project structure

## Projects in this repository

### Falanx.Tool
A simple console application to produce an F# source file from the `.proto` files.

### Falanx.Sdk
Integration with .NET Sdk projects

### Falanx.Templates
The templates who contains an example library

## Dependencies

### Type Provider SDK
This project allows you to build type providers by leveraging quotations and wrappers around Types, MethodInfo, PropertyInfo, etc., to generate and inject CIL (Common Intermediate Language) into a target assembly.  Falanx does not use the type provider generator mechanism, only the skeletal structure and definitions.  This allows us to save a lot of time by reusing and adapting already written quotation code that was build for Froto.TypeProvider.

### FsAst
This was a proof of concept demo using the untyped F# AST to generate code via the code formatter Fantomas.

### Froto.Serialization
This generates binary protocol for `proto` specific fields.

### Froto.Parser
This parses the `.proto` definition files to produce an AST.

### Fantomas
This is an F# code formatter assembly that format F# code and can turn an F# AST back into code.


# How to build Falanx

Use the `sln/Falanx.sln` solution for development, or directly the projects with .NET Core Sdk (`dotnet`).

Projects:

- `src/Falanx.Tool` the falanx console app, run with `dotnet run`
- `test/Falanx.Tests` the unit test, run with `dotnet run`
- `test/Falanx.IntegrationTests` the integration tests, run with `dotnet run`

As shortcuts, from root:

- `dotnet build` to build the falanx executable `Falanx.Tool`.
- `dotnet pack` to generate packages in `bin/nupks`
- `dotnet test -v n` to run tests

# Using Falanx

## Generating code from a .proto file

From the root folder, to generate a `.fs` file for a specified `.proto` file:

```
dotnet run -p src/Falanx.Tool/Falanx.Tool.fsproj -- --inputfile test\examples\schemas\bundle.proto --defaultnamespace test --outputfile bundle.fs
```

Command line arguments can be shown by calling with  `--help`:

```
USAGE: dotnet-Falanx.Tool [--help] --inputfile <string> --defaultnamespace <string> --outputfile <string>

OPTIONS:
    --inputfile <string>  specify a proto file to input.
    --defaultnamespace <string> specify a default namespace to use for code generation.
    --outputfile <string> Specify the file name that the generated code will be written to.
    --help                display this list of options.
```

## To build packages

From root

```
dotnet pack
```

The nupkgs will be in `bin/nupkg`

To specify a version pass the `Version` property like `/p:Version=0.1.0-alpha7`

## Security
This repository is actively monitored by Jet Engineers and the Jet Security team. Please monitor this repo for security updates and advisories. For more information and contacts, please see [SECURITY](security.md)



