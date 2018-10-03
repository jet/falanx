# How to use the generator inside a project

Falax has been built to allow it to integrate into .NET Sdk projects, this example shows how this works.

## Usage with nuget

Essentially you need to add the following packages into an `ItemGroup` of your project file.

```
<PackageReference Include="Falanx.BinaryCodec" Version="0.2.0" />
<PackageReference Include="Falanx.Sdk" Version="0.2.0" PrivateAssets="All" />
<DotNetCliToolReference Include="dotnet-Falanx.BinaryGenerator" Version="0.2.0" />
```

and configure the source path of the proto file in a

```
<ProtoFile Include="..\proto\bundle.proto" />
```

## Usage

The proto file specified is used to generate a temporary `.fs` source file, automatically added to the project.

```
dotnet build l1
dotnet run -p l1
```
