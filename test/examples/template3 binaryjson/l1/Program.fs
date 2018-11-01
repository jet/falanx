namespace l1

module Program =

    [<EntryPoint>]
    let main args =

        let bytes = BinaryExample.serialize ()

        printfn "[BINARY] Serialized:"
        printfn "%A" bytes

        let s = BinaryExample.deserialize bytes

        printfn "[BINARY] Deserialized:"
        printfn "%A" s

        let bytes = JsonExample.serialize ()

        printfn "[JSON] Serialized:"
        printfn "%A" bytes

        let s = JsonExample.deserialize bytes

        printfn "[JSON] Deserialized:"
        printfn "%A" s

        0

