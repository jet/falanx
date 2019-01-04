module JsonExample

open l1.Contracts
open Fleece.Newtonsoft

let serialize () : string =
    let r =
        { BundleRequest.martId = Some 1
          memberId =  Some "myId"
          channelType = None
          retailSkus = new ResizeArray<string>() }

    toJson r |> string

let deserialize (jsonText: string) : BundleRequest =

    let s = parseJson jsonText

    match s with
    | Result.Ok x -> x
    | Result.Error err -> failwithf "error parsing json: '%A'" err
