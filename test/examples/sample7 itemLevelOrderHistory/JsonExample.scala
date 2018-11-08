class JsonExample {

  def serialize() = {
    val item =
      ItemLevelOrderHistory()
        .withClientId("client1")
        .withRetailSkuId("sku1")
        .withCategoryId(12.3)
        .withBrand("brandA")
        .withProduct("product1")
        .withOrderTss(45.6f)

    val jsonText: String = scalapb.json4s.JsonFormat.toJsonString(item)
    jsonText
  }

  def deserialize(jsonText: String) = {
    val item2: ItemLevelOrderHistory = scalapb.json4s.JsonFormat.fromJsonString[ItemLevelOrderHistory](jsonText)
    item2
  }
}
