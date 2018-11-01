module BinaryExample

open l1.Contracts

let serialize () =
    let r =
        { BundleRequest.martId = Some 1
          memberId =  Some "myId"
          channelType = None
          retailSkus = new ResizeArray<string>() }

    let buffer = new Froto.Serialization.ZeroCopyBuffer(1000)
    BundleRequest.Serialize(r, buffer)
    buffer

let deserialize (bytes: byte array) =
    let buffer2 = new Froto.Serialization.ZeroCopyBuffer(bytes)
    let s = BundleRequest.Deserialize(buffer2)
    s
