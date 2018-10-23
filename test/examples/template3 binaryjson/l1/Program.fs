namespace l1

module Program =

    [<EntryPoint>]
    let main args =

        let buffer = BinaryExample.serialize ()

        printfn "[BINARY] Serialized:"
        printfn "%A" (buffer.Array)

        let s = BinaryExample.deserialize buffer.Array

        printfn "[BINARY] Deserialized:"
        printfn "%A" s

        let buffer = JsonExample.serialize ()

        printfn "[JSON] Serialized:"
        printfn "%A" (buffer.Array)

        let s = JsonExample.deserialize buffer.Array

        printfn "[JSON] Deserialized:"
        printfn "%A" s

        0

