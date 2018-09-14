# How to use the proto generator as a CLI tool

Protogenerator has been built to allow it to integrate into the CLI of .NET, this example shows how this works.

## Usage with nuget

Essentially you need to add a `DotNetCliToolReference` by adding the following `ItemGroup` to your project file.

```
<DotNetCliToolReference Include="dotnet-protoGenerator" Version="0.1.0-alpha" />
```
## Usage

```
cd tools
dotnet restore
dotnet protoGenerator
```

## Usage with paket

Paket also has a way of providing this functionality by adding the following to you paket.references and paket.dependencies files:

### paket.dependencies
```
clitool dotnet-protoGenerator 0.1.0-alpha
```

### paket.reference
```
clitool dotnet-protoGenerator
```


## sample1

```
cd tools
dotnet restore
dotnet protoGenerator --inputfile ../sample1/bundle.proto --defaultnamespace MyNamespace --outputfile bundle.fs
```


