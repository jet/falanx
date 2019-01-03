module JsonExample

open l1.Contracts
open Fleece.Newtonsoft

let serialize () : string =
    let r =
        { ItemLevelOrderHistory.clientId = Some "clientA"
          retailSkuId = Some "sku12345"
          categoryId = Some 78.91
          brand = Some "myBrand1"
          product = Some "p100"
          orderTss = Some 43.21f }

    toJson r |> string

let deserialize (jsonText: string) : ItemLevelOrderHistory =

    let s = parseJson jsonText

    match s with
    | Result.Ok x -> x
    | Result.Error err -> failwithf "error parsing json: '%A'" err
    
