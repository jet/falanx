namespace l1

open l1.Contracts

module Program =

    [<EntryPoint>]
    let main args =
        let r =
            { BundleRequest.martId = Some 1
              memberId =  Some "myId"
              channelType = None
              retailSkus = new ResizeArray<string>() }

        let buffer = new Froto.Serialization.ZeroCopyBuffer(1000);
        BundleRequest.Serialize(r, buffer)

        printfn "Serialized:"
        printfn "%A" (buffer.Array)

        let buffer2 = new Froto.Serialization.ZeroCopyBuffer(buffer.Array);
        let s = BundleRequest.Deserialize(buffer2)

        printfn "Deserialized:"
        printfn "%A" s

        0

