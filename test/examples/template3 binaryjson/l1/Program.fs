namespace l1

open System.IO

module Program =

    type Command =
        | Serialize of string * string
        | Deserialize of string * string * string

    [<EntryPoint>]
    let main args =

        let cmd =
            match args with
            | [| "--format"; format; "--serialize"; path |] -> Serialize (format, path)
            | [| "--format"; format; "--deserialize"; path; "--out"; out |] -> Deserialize (format, path, out)
            | _ -> failwithf "invalid args, expecting --serialize or --deserialize, but was '%A'" args

        match cmd with
        | Serialize ("binary", path) ->
            let bytes = BinaryExample.serialize ()

            printfn "Serialized:"
            printfn "%A" bytes

            File.WriteAllBytes(path, bytes)
            0
        | Deserialize ("binary", path, outputPath) ->
            let bytes = File.ReadAllBytes(path)

            let s = BinaryExample.deserialize bytes

            printfn "Deserialized:"
            printfn "%A" s

            File.WriteAllText(outputPath, sprintf "%A" s, System.Text.Encoding.UTF8)
            0
        | Serialize ("json", path) ->
            let bytes = JsonExample.serialize ()

            printfn "Serialized:"
            printfn "%A" bytes

            File.WriteAllText(path, bytes)
            0
        | Deserialize ("json", path, outputPath) ->
            let jsonText = File.ReadAllText(path)

            let s = JsonExample.deserialize jsonText

            printfn "Deserialized:"
            printfn "%A" s

            File.WriteAllText(outputPath, sprintf "%A" s, System.Text.Encoding.UTF8)
            0
        | Serialize (format, _) ->
            printfn "format %s not supported, avaiaible 'json' or 'binary'" format
            2
        | Deserialize (format, _, _) ->
            printfn "format %s not supported, avaiaible 'json' or 'binary'" format
            2
