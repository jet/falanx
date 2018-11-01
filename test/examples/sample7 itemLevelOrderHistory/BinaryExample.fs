module BinaryExample

open l1.Contracts

let serialize () =
    let r =
        { ItemLevelOrderHistory.clientId = Some "clientA"
          retailSkuId = Some "sku12345"
          categoryId = Some 78.91
          brand = Some "myBrand1"
          product = Some "p100"
          orderTss = Some 43.21f }

    let buffer = new Froto.Serialization.ZeroCopyBuffer(1000)
    ItemLevelOrderHistory.Serialize(r, buffer)
    buffer.ToArray()

let deserialize (bytes: byte array) =
    let buffer2 = new Froto.Serialization.ZeroCopyBuffer(bytes)
    let s = ItemLevelOrderHistory.Deserialize(buffer2)
    s
