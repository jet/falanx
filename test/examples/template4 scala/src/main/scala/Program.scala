import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets

object Program {

  sealed trait Command
  case class Serialize(path: String) extends Command
  case class Deserialize(path: String, out: String) extends Command


  def main(args: Array[String]): Unit = {

    val cmd =
      args match {
        case Array("--serialize", p) => Serialize(p)
        case Array("--deserialize", p, "--out", o) => Deserialize(p, o)
        case _ => throw new Exception("invalid args, expecting --serialize or --deserialize")
      }

    val example = new BinaryExample

    cmd match {
      case Serialize(path) => {
        val bytes = example.serialize()
        println(bytes)
        Files.write(Paths.get(path), bytes)
      }
      case Deserialize(path, out) => {
        val bytes = Files.readAllBytes(Paths.get(path))
        val item2 = example.deserialize(bytes)
        println(item2)
        Files.write(Paths.get(out), item2.toString().getBytes(StandardCharsets.UTF_8))
      }
    }

  }
}
