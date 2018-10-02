// Learn more about F# at http://fsharp.org
namespace Falanx.Generator
open System
open System.IO
open Argu
open Falanx.BinaryGenerator
open Falanx.JsonCodec

module main =

    type Arguments =
        | [<Mandatory>] InputFile of string
        | [<Mandatory>] DefaultNamespace of string
        | [<Mandatory>] OutputFile of string
    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | InputFile _ -> "specify a proto file to input."
                | DefaultNamespace _ -> "specify a default namespace to use for code generation."
                | OutputFile _ -> "Specify the file name that the generated code will be written to."
    
    
    let parser = ArgumentParser.Create<Arguments>(programName = "dotnet-Falanx.Generator")
    
    [<EntryPoint>]
    let main argv =
        try
            let code = temp.tryCode()
                        
            let results = parser.Parse argv
            let inputFile = results.GetResult InputFile
            let outputFile = results.GetResult OutputFile
            let defaultNamespace = results.GetResult DefaultNamespace
            let protoDef = IO.File.ReadAllText inputFile
            printfn "Generating code for: %s" inputFile
            Proto.createFSharpDefinitions(protoDef, outputFile, defaultNamespace)
        with
        | :? FileNotFoundException as fnf ->
            printfn "ERROR: inputfile %s doesn not exist\n%s" fnf.FileName (parser.PrintUsage())
        | ex -> printfn "%s\n%s" (parser.PrintUsage()) ex.Message
    
        0 // return an integer exit code
