[![Build Status](https://travis-ci.org/jet/falanx.svg?branch=master)](https://travis-ci.org/jet/falanx)
[![Build status](https://ci.appveyor.com/api/projects/status/rgnou677cvs890nt/branch/master?svg=true)](https://ci.appveyor.com/project/Jet/falanx/branch/master)

# Falanx Proto3 code generation

This repository contains the code generator to generate F# source (.fs files) from `proto3` source.  The general concepts are as follows:

*  Resuse as much off the shelf code as possible for the mvp as to get a feel for how things will work and refine from there.
*  Code generation, to generate F# source code, rather than types being injected as a type provider.
*  Idiomatic F# code is generated rather than simple .NET 1.1 era code. This means records, discriminated unions, etc., are generated where appropriate.

# Project structure

The solution has four projects.

## Projects in this repository

### Falanx.Ast
This maps ProvidedTypes, Methods, and Properties that are defined in the Type Provider SDK to AST entities.

### Falanx.BinaryCodec
This is a helper project for the client to serialize and deserialize code.

### Falanx.BinaryGenerator
<TODO: Write a desciption>

### Falanx.Generator
The generator is a simple command line program which uses the other projects to produce F# code from the `.proto` files.

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

This is a simple project, so there is no fake script as no elaborate build is yet required. All you have to do is `dotnet build Falanx.sln` from the root folder. The default solution will have its references restored via `paket` and then built.

# Using Falanx

## Generating code from a .proto file

From the root folder, to generate a `.fs` file for a specified `.proto` file:

```
dotnet run --project Falanx.Generator/Falanx.Generator.fsproj --inputfile bundle.proto --defaultnamespace test --outputfile bundle.fs
```

Command line arguments can be shown by calling with  `--help`:

```
USAGE: dotnet-Falanx.Generator [--help] --inputfile <string> --defaultnamespace <string> --outputfile <string>

OPTIONS:
    --inputfile <string>  specify a proto file to input.
    --defaultnamespace <string> specify a default namespace to use for code generation.
    --outputfile <string> Specify the file name that the generated code will be written to.
    --help                display this list of options.
```


## To build packages

From root

```
dotnet msbuild build.proj /t:Pack /p:Version=0.1.0-alpha7
```

The nupkg will be in `artifact/nupkg`  

## Security
This repository is actively monitored by Jet Engineers and the Jet Security team. Please monitor this repo for security updates and advisories. For more information and contacts, please see [SECURITY](security.md)



