module Falanx.Program

open Expecto
open System
open System.IO

[<EntryPoint>]
let main argv =
    let artifactsDir =
        IO.Path.Combine(__SOURCE_DIRECTORY__,"..","..","artifact")
        |> Path.GetFullPath
    let nupkgsDir = IO.Path.Combine(artifactsDir,"nupkg")

    let findPackedVersion () =
        if not (Directory.Exists nupkgsDir) then
            None
        else
            nupkgsDir
            |> Directory.EnumerateFiles
            |> Seq.map Path.GetFileNameWithoutExtension
            |> Seq.tryFind (fun p -> p.StartsWith("Falanx.Tool"))
            |> Option.map (fun p -> p.Replace("Falanx.Tool.",""))

    let info =
        match argv |> List.ofArray with
        | pkgUnderTestVersion :: args when Char.IsNumber(pkgUnderTestVersion.[0]) ->
            printfn "testing package: %s" pkgUnderTestVersion
            Ok (pkgUnderTestVersion, args)
        | args ->
            printfn "Package version not passed as first argument, searching in nupks dir"
            match findPackedVersion () with
            | Some v ->
                printfn "found version '%s' of Falanx.Tool" v
                Ok (v, args)
            | None ->
                printfn "Falanx.Tool nupkg not found in '%s'" nupkgsDir
                Error 1

    match info with
    | Error exitCode ->
        printfn "expected package version as first argument, or Falanx.Tool nupkg in dir '%s'" nupkgsDir
        exitCode
    | Ok (pkgUnderTestVersion, args) ->
        printfn "testing package: %s" pkgUnderTestVersion

        Environment.SetEnvironmentVariable("MSBuildExtensionsPath", null)

        let resultsPath = IO.Path.Combine(artifactsDir,"test_results","TestResults.xml")

        let writeResults = TestResults.writeNUnitSummary (resultsPath, "Falanx.IntegrationTests")
        let config = defaultConfig.appendSummaryHandler writeResults

        Tests.runTestsWithArgs config (args |> Array.ofList) (Falanx.Tests.tests pkgUnderTestVersion)
