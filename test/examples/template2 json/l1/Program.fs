namespace l1

module Program =

    [<EntryPoint>]
    let main args =

        let buffer = JsonExample.serialize ()

        printfn "Serialized:"
        printfn "%A" (buffer.Array)

        let s = JsonExample.deserialize buffer.Array

        printfn "Deserialized:"
        printfn "%A" s

        0
