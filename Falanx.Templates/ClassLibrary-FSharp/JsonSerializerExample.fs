namespace Company.ClassLibrary1

module JsonSerializerExample =

    open Fleece.Newtonsoft

    let serialize () : string =
        let r =
            { BundleRequest.martId = Some 1
              memberId =  Some "myId"
              channelType = None
              retailSkus = Some "sdk-1" }

        toJson r |> string

    let deserialize (jsonText: string) : ParseResult<BundleRequest> =
        parseJson jsonText
