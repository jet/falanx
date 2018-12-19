module Falanx.Program

open Expecto
open System
open System.IO

[<EntryPoint>]
let main argv =
    let artifactsDir =
        IO.Path.Combine(__SOURCE_DIRECTORY__,"..","..","artifact")
        |> Path.GetFullPath

    let resultsPath = IO.Path.Combine(artifactsDir,"test_results","TestResults.Unit.xml")

    let writeResults = TestResults.writeNUnitSummary (resultsPath, "Falanx.Tests")
    let config = defaultConfig.appendSummaryHandler writeResults

    Tests.runTestsWithArgs config argv (Falanx.UnitTests.tests ())
