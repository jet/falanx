# Developer Guide

## Prerequisites

In order to build this repo, you need the following installed on you machine:

- .NET Core Sdk, see [global.json](..\global.json) for the exact version

## Project structure

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
