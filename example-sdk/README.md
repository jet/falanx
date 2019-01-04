# How to use the generator inside a project

Falax can be integrated in .NET Sdk projects using `Falanx.Sdk` package.

## Usage with nuget

Add the following packages into an `ItemGroup` of your project file.

```xml
<PackageReference Include="Falanx.Proto.Codec.Binary" Version="0.4.0" />
<PackageReference Include="Falanx.Proto.Codec.Json" Version="0.4.0" />
<PackageReference Include="Falanx.Sdk" Version="0.4.0" PrivateAssets="All" />
```

and configure the source path of the proto file in a

```xml
<ProtoFile Include="..\proto\bundle.proto" />
```

**NOTE** It's possibile to use only one of `Falanx.Proto.Codec.Binary` and `Falanx.Proto.Codec.Json` or both. The generated code will depends on the packages referenced

The proto file specified is used to generate a `.fs` source file alongside the `.proto` file, automatically added to the project.

To configure the generated source `.fs` file path, use the `OutputPath` metadata, like

```xml
<ProtoFile Include="schemas\bundle.proto">
    <OutputPath>mycustom.fs</OutputPath>
</ProtoFile>
```

The `.fs` source file with be regenerated on build if the proto file is changed.

## Template

```
dotnet new -i Falanx.Templates
dotnet new falanx
```

use `--codec` argument to specify the codecs (values `json`,`binary`,`all`)
