namespace Company.ClassLibrary1

module BinarySerializerExample =

    let serialize () =
        let r =
            { BundleRequest.martId = Some 1
              memberId =  Some "myId"
              channelType = None
              retailSkus = Some "sdk-1" }

        let buffer = new Froto.Serialization.ZeroCopyBuffer(1000)
        BundleRequest.Serialize(r, buffer)
        buffer.ToArray()

    let deserialize (bytes: byte array) =
        let buffer = new Froto.Serialization.ZeroCopyBuffer(bytes)
        BundleRequest.Deserialize(buffer)
