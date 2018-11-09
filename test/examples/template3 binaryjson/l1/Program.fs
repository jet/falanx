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

        let jsonText = JsonExample.serialize ()

        printfn "[JSON] Serialized:"
        printfn "%A" jsonText

        let s = JsonExample.deserialize jsonText

        printfn "[JSON] Deserialized:"
        printfn "%A" s

        0

