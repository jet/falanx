module Falanx.TestAssets

open FileUtils

type TestAssetProjInfo =
  { ProjDir: string
    ProjectFile: string
    AssemblyName: string
    ProtoFile: string }

let ``samples2 library`` =
  { ProjDir = "sample2"
    ProjectFile = "l1"/"l1.fsproj"
    AssemblyName = "l1"
    ProtoFile = "proto"/"bundle.proto" }

type FalanxMock =
  { ProjDir: string
    FileName: string }

let ``mock write args`` =
  { ProjDir = "mock1"
    FileName = "falanx-mock" }
