module Falanx.Program

open Expecto
open System
open System.IO

let tests () =
  [ GoogleProtobufTest.V3.CompatibilityTests.tests
    Falanx.Tests.tests ]
  |> testList "unit"
  |> testSequenced

[<EntryPoint>]
let main argv =
    let artifactsDir =
        IO.Path.Combine(__SOURCE_DIRECTORY__,"..","..","bin")
        |> Path.GetFullPath

    let resultsPath = IO.Path.Combine(artifactsDir,"test_results","TestResults.Unit.xml")

    let writeResults = TestResults.writeNUnitSummary (resultsPath, "Falanx.Tests")
    let config = defaultConfig.appendSummaryHandler writeResults

    Tests.runTestsWithArgs config argv (tests ())
