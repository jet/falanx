import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets
import scala.collection.JavaConverters._

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

    val example = new JsonExample

    cmd match {
      case Serialize(path) => {
        val jsonText = example.serialize()
        println(jsonText)
        val bytes = jsonText.getBytes(StandardCharsets.UTF_8)
        Files.write(Paths.get(path), bytes)
      }
      case Deserialize(path, out) => {
        val lines = Files.readAllLines(Paths.get(path)).asScala.toList
        val jsonText = lines.mkString(System.lineSeparator())
        val item2 = example.deserialize(jsonText)
        println(item2)
        Files.write(Paths.get(out), item2.toString().getBytes(StandardCharsets.UTF_8))
      }
    }

  }
}
