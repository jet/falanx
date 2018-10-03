# How to use the falanx generator as a CLI tool

Falanx generator has been built to allow it to integrate into the CLI of .NET, this example shows how this works.

## Usage with nuget

Essentially you need to add a `DotNetCliToolReference` by adding the following `ItemGroup` to your project file.

```
<DotNetCliToolReference Include="dotnet-Falanx.BinaryGenerator" Version="0.2.0" />
```
## Usage

```
cd tools
dotnet restore
dotnet Falanx.BinaryGenerator --help
```

## Usage with paket

Paket also has a way of providing this functionality by adding the following to you paket.references and paket.dependencies files:

### paket.dependencies
```
clitool dotnet-Falanx.BinaryGenerator 0.2.0
```

### paket.reference
```
clitool dotnet-Falanx.BinaryGenerator
```


## sample1

```
cd tools
dotnet restore
dotnet Falanx.BinaryGenerator --inputfile ../sample1/bundle.proto --defaultnamespace MyNamespace --outputfile bundle.fs
```


