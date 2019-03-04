namespace Falanx.Generator
open System
open System.IO
open Argu
open Falanx.Proto.Generator
open Falanx.Proto.Generator.TypeGeneration

module main =

    type Arguments =
        | [<Mandatory>] InputFile of string
        | DefaultNamespace of string
        | [<Mandatory>] OutputFile of string
        | Serializer of Codec
        | [<Hidden>]Wait_Debugger
    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | InputFile _ -> "specify a proto file to input."
                | DefaultNamespace _ -> "specify a default namespace to use for code generation."
                | OutputFile _ -> "Specify the file name that the generated code will be written to."
                | Serializer _ -> "serialization format. default binary"
                | Wait_Debugger -> "Waits for a debugger to attach before proceeding."

    [<EntryPoint>]
    let main argv =
        let parser = ArgumentParser.Create<Arguments>(programName = "falanx")

        try

            let results = parser.Parse argv
            let inputFile = results.GetResult InputFile
            let outputFile = results.GetResult OutputFile
            let defaultNamespace =
                match results.TryGetResult DefaultNamespace with
                | Some ns -> ns
                | None -> Path.GetFileNameWithoutExtension(inputFile)
            let codecs =
                match results.GetResults Serializer with
                | [] -> Set.singleton Binary
                | l -> l |> Set.ofList
            let protoDef = IO.File.ReadAllText inputFile
            printfn "Generating code for: %s" inputFile
            Proto.createFSharpDefinitions(protoDef, outputFile, defaultNamespace, codecs)
            0
        with
        | :? ArguParseException as ae ->
            printfn "%s" ae.Message
            match ae.ErrorCode with
            | Argu.ErrorCode.HelpText -> 0
            | _ -> 2
        | :? ArguParseException as ae when ae.ErrorCode = Argu.ErrorCode.HelpText ->
            printfn "%s" ae.Message
            3
        | :? FileNotFoundException as fnf ->
            printfn "ERROR: inputfile %s doesn not exist\n%s" fnf.FileName (parser.PrintUsage())
            4
        | :? FormatException as fex when fex.Source = "Froto.Parser" ->
            printfn "ERROR: proto file was not able to be parsed.\n\n%s" fex.Message
            5
        | ex ->
            printfn "%s\n%s" (parser.PrintUsage()) ex.Message
            1
