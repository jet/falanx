module Falanx.TestAssets

open FileUtils

type TestAssetProjInfo =
  { ProjDir: string
    ProjectFile: string
    AssemblyName: string
    ProtoFile: string }

let ``samples2 binary`` =
  { ProjDir = "sample2"
    ProjectFile = "l1"/"l1.fsproj"
    AssemblyName = "l1"
    ProtoFile = "proto"/"bundle.proto" }

let ``samples3 json`` =
  { ProjDir = "sample3 json"
    ProjectFile = "l1"/"l1.fsproj"
    AssemblyName = "l1"
    ProtoFile = "proto"/"bundle.proto" }

let ``sample4 binary+json`` =
  { ProjDir = "sample4 binaryjson"
    ProjectFile = "l1"/"l1.fsproj"
    AssemblyName = "l1"
    ProtoFile = "proto"/"bundle.proto" }

type FalanxMock =
  { ProjDir: string
    FileName: string }

let ``mock write args`` =
  { ProjDir = "mock1"
    FileName = "falanx-mock" }
