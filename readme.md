# schematization

This repository deal with the code generation needed for the schematization project.  The general concepts are as follows:

*  Resuse as much off the shelf code as possible for the mvp as to get a feel for how things will work and refine from there.
*  Code generation where code exists rather than being injected as it is with a type provider.
*  Idiomatic F# code is generated rather than simple .NET 1.1 era code, this means records, discriminated unions etc are generated where appropriate.

# Project structure

The solution is relatively simple with four projects

## Projects in this repository

### Falanx.Ast
This deals with helping to map ProvidedTypes/Methods/Properties that are defined in the Type Provider SDK and map them to AST entities

### Falanx.BinaryCodec
This project acts as a helper project for the client in order to serialize and deserialize code

### Falanx.BinaryGenerator

### Falanx.Generator
The generator is a simple command line program which references all the other projects in order to produce F# code from the `.proto` files.

## Upstream projects

### Type Provider SDK
This project allows you to build type providers by leveridging quotations and wrappers arounf Type/MethodInfo/PropertyInfo etc in order to generate and inject IL into a target assembly.  We are not using the type provider generator mechanism, omly its skeletal structure and definitions.  This allows us to save a lot of time by reusing and adapting already written quotation code that was build for Froto.TypeProvider.

### FsAst
This was a proof of concept demo of using the untyped F# ast to generate code via the code formatter Fantomas.

### Froto.Serialization
This generates binary protocol for proto specific fields.

### Froto.Parser
This parses the `.proto` definition files to produce an AST.

### Fantomas
This is an F# code formatter assembly that can not only format F# code but also F# AST's back into code.


# How to build

This is a simple project there is not yet a fake script as no elaborate build is yet required, all you have to do is `dotnet build Falanx.sln` from the root folder and the default solution should have its references restored via paket and then built.

## Generating code from a .protofile

From the root folder running the following will generate a `.fs` file for a specified `.proto` file:

```
dotnet run --project Falanx.Generator/Falanx.Generator.fsproj --inputfile bundle.proto --defaultnamespace test --outputfile bundle.fs
```

Command line arguments are as follows, these can also be shown by passing `--help`:

```
USAGE: dotnet-Falanx.Generator [--help] --inputfile <string> --defaultnamespace <string> --outputfile <string>

OPTIONS:
    --inputfile <string>  specify a proto file to input.
    --defaultnamespace <string> specify a default namespace to use for code generation.
    --outputfile <string> Specify the file name that the generated code will be written to.
    --help                display this list of options.
```


## to build packages

from root

```
dotnet msbuild build.proj /t:Pack /p:Version=0.1.0-alpha7
```

and nupkg will be in `artifact/nupkg`  

## Security
This repository is actively monitored by Jet Engineers and the Jet Security team. Please monitor this repo for security updates and advisories. For more information and contacts, please see [SECURITY](security.md)



