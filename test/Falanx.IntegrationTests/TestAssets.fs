module Falanx.TestAssets

open FileUtils

type FormatKind =
  | Binary
  | Json

type Language =
  | FSharp
  | Scala

type TestAssetProjInfo =
  { ProjDir: string
    Language: Language
    RequiredFormats: FormatKind list
    ProjectFile: string
    AssemblyName: string
    ProtoFile: string }

let ``template1 binary`` =
  { ProjDir = "template1"
    Language = FSharp
    RequiredFormats = [Binary]
    ProjectFile = "l1"/"l1.fsproj"
    AssemblyName = "l1"
    ProtoFile = "proto"/"bundle.proto" }

let ``template2 json`` =
  { ProjDir = "template2 json"
    Language = FSharp
    RequiredFormats = [Json]
    ProjectFile = "l1"/"l1.fsproj"
    AssemblyName = "l1"
    ProtoFile = "proto"/"bundle.proto" }

let ``template3 binary+json`` =
  { ProjDir = "template3 binaryjson"
    Language = FSharp
    RequiredFormats = [Binary; Json]
    ProjectFile = "l1"/"l1.fsproj"
    AssemblyName = "l1"
    ProtoFile = "proto"/"bundle.proto" }

let ``template4 scala`` =
  { ProjDir = "template4 scala"
    Language = Scala
    RequiredFormats = [Binary]
    ProjectFile = "src"/"main"/"scala"/"Program.scala"
    AssemblyName = "src"/"main"/"scala"
    ProtoFile = "src"/"main"/"protobuf"/"ItemLevelOrderHistory.proto" }

type FalanxMock =
  { ProjDir: string
    FileName: string }

let ``mock write args`` =
  { ProjDir = "mock1"
    FileName = "falanx-mock" }

type TestAssetExampleInfo =
  { ExampleDir: string
    FileNames: (Language * FormatKind * string) list
    ProtoFile: string }

let ``sample5 pkg`` =
  { ExampleDir = "sample5 pkg"
    FileNames = [FSharp, Binary, "BinaryExample.fs"]
    ProtoFile = "bundle.proto" }

let ``sample6 bundle`` =
  { ExampleDir = "sample6 bundle"
    FileNames = [FSharp, Binary, "BinaryExample.fs"; FSharp, Json, "JsonExample.fs"]
    ProtoFile = "bundle.proto" }

let ``sample7 itemLevelOrderHistory`` =
  { ExampleDir = "sample7 itemLevelOrderHistory"
    FileNames = [FSharp, Binary, "BinaryExample.fs"; FSharp, Json, "JsonExample.fs"; Scala, Binary, "BinaryExample.scala"]
    ProtoFile = "ItemLevelOrderHistory.proto" }
