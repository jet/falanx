object Program extends App {
  println("Hello, World!")

  val example = new BinaryExample

  val bytes = example.serialize()
  println(bytes)

  var item2 = example.deserialize(bytes)

  println(item2)

}
