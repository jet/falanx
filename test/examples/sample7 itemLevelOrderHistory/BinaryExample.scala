class BinaryExample {

  def serialize() = {
    val item =
      ItemLevelOrderHistory()
        .withClientId("client1")
        .withRetailSkuId("sku1")
        .withCategoryId(12.3)
        .withBrand("brandA")
        .withProduct("product1")
        .withOrderTss(45.6f)

    val bytes = item.toByteArray
    bytes
  }

  def deserialize(bytes: Array[Byte]) = {
    val item2 = ItemLevelOrderHistory.parseFrom(bytes)
    item2
  }
}
