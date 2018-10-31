module Falanx.TestAssets

open FileUtils

type TestAssetProjInfo =
  { ProjDir: string
    ProjectFile: string
    AssemblyName: string
    ProtoFile: string }

let ``template1 binary`` =
  { ProjDir = "template1"
    ProjectFile = "l1"/"l1.fsproj"
    AssemblyName = "l1"
    ProtoFile = "proto"/"bundle.proto" }

let ``template2 json`` =
  { ProjDir = "template2 json"
    ProjectFile = "l1"/"l1.fsproj"
    AssemblyName = "l1"
    ProtoFile = "proto"/"bundle.proto" }

let ``template3 binary+json`` =
  { ProjDir = "template3 binaryjson"
    ProjectFile = "l1"/"l1.fsproj"
    AssemblyName = "l1"
    ProtoFile = "proto"/"bundle.proto" }

let ``template4 scala`` =
  { ProjDir = "template4 scala"
    ProjectFile = "l1"/"l1.fsproj"
    AssemblyName = "l1"
    ProtoFile = "src"/"main"/"protobuf"/"ItemLevelOrderHistory.proto" }

type FalanxMock =
  { ProjDir: string
    FileName: string }

let ``mock write args`` =
  { ProjDir = "mock1"
    FileName = "falanx-mock" }

type TestAssetExampleInfo =
  { ExampleDir: string
    FileNames: string list
    ProtoFile: string }

let ``sample2 binary`` =
  { ExampleDir = "sample2 binary"
    FileNames = ["BinaryExample.fs"]
    ProtoFile = "bundle.proto" }

let ``sample3 json`` =
  { ExampleDir = "sample3 json"
    FileNames = ["JsonExample.fs"]
    ProtoFile = "bundle.proto" }

let ``sample4 binary+json`` =
  { ExampleDir = "sample4 binaryjson"
    FileNames = ["BinaryExample.fs"; "JsonExample.fs"]
    ProtoFile = "bundle.proto" }

let ``sample5 pkg`` =
  { ExampleDir = "sample5 pkg"
    FileNames = ["BinaryExample.fs"]
    ProtoFile = "bundle.proto" }
