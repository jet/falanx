namespace l1

open System.IO

module Program =

    type Command =
        | Serialize of string
        | Deserialize of string
        | SerializeDeserialize

    [<EntryPoint>]
    let main args =

        let cmd =
            match args with
            | [| "--serialize"; path |] -> Serialize path
            | [| "--deserialize"; path |] -> Deserialize path
            | [| |] -> SerializeDeserialize
            | _ -> failwith "invalid args, expecting --serialize or --deserialize"

        match cmd with
        | SerializeDeserialize ->
            let bytes = BinaryExample.serialize ()

            printfn "Serialized:"
            printfn "%A" bytes

            let s = BinaryExample.deserialize bytes

            printfn "Deserialized:"
            printfn "%A" s

        | Serialize path ->
            let bytes = BinaryExample.serialize ()

            printfn "Serialized:"
            printfn "%A" bytes

            File.WriteAllBytes(path, bytes)

        | Deserialize path ->
            let bytes = File.ReadAllBytes(path)

            let s = BinaryExample.deserialize bytes

            printfn "Deserialized:"
            printfn "%A" s

        0

