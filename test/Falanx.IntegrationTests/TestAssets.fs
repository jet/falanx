module Falanx.TestAssets

open FileUtils

type TestAssetProjInfo =
  { ProjDir: string
    ProjectFile: string
    AssemblyName: string }

let ``samples2 library`` =
  { ProjDir = "sample2"
    ProjectFile = "l1"/"l1.fsproj"
    AssemblyName = "l1" }
