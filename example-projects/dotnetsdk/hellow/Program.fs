// Learn more about F# at http://fsharp.org

open System
open aaa

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"

    let r =
        { BundleRequest.martId = Some 1
          MemberId =  Some "myId"
          channelType = None
          retailSkus = new ResizeArray<string>() }

    let buffer = new Froto.Serialization.ZeroCopyBuffer(1000);
    BundleRequest.Serialize(r, buffer)

    printfn "%A" (buffer.Array)

    0 // return an integer exit code
