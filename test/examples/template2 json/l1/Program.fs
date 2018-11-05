namespace l1

open System.IO

module Program =

    type Command =
        | Serialize of string
        | Deserialize of string * string

    [<EntryPoint>]
    let main args =

        let cmd =
            match args with
            | [| "--serialize"; path |] -> Serialize path
            | [| "--deserialize"; path; "--out"; out |] -> Deserialize (path, out)
            | _ -> failwithf "invalid args, expecting --serialize or --deserialize, but was '%A'" args

        match cmd with
        | Serialize path ->
            let bytes = JsonExample.serialize ()

            printfn "Serialized:"
            printfn "%A" bytes

            File.WriteAllText(path, bytes)

        | Deserialize (path, outputPath) ->
            let jsonText = File.ReadAllText(path)

            let s = JsonExample.deserialize jsonText

            printfn "Deserialized:"
            printfn "%A" s

            File.WriteAllText(outputPath, sprintf "%A" s, System.Text.Encoding.UTF8)
        0

