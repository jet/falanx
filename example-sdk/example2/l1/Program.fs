namespace l1

open l1.Contracts

module Program =

    [<EntryPoint>]
    let main args =
        let r =
            { BundleRequest.martId = Some 1
              MemberId =  Some "myId"
              channelType = None
              retailSkus = new ResizeArray<string>() }

        let buffer = new Froto.Serialization.ZeroCopyBuffer(1000);
        BundleRequest.Serialize(r, buffer)

        printfn "%A" (buffer.Array)

        0

