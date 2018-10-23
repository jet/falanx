namespace l1

module Program =

    [<EntryPoint>]
    let main args =

        let buffer = BinaryExample.serialize ()

        printfn "Serialized:"
        printfn "%A" (buffer.Array)

        let s = BinaryExample.deserialize buffer.Array

        printfn "Deserialized:"
        printfn "%A" s

        0

