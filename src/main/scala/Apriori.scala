import scala.io.Source
import scala.util.{Failure, Success, Try}

object Apriori extends App {
  type ItemSet = List[Set[String]]

  val data = "data/T10I4D100K.dat"
  run(data, 0.5)

  def run(file: String, support: Double): Unit = {
    getItemSet(data) match {
      case Success(set) => println("yay")
      case Failure(e) => println(e.getMessage)
    }
  }

  def getItemSet(file: String): Try[ItemSet] = Try {
    Source.fromFile(file)
      .getLines()
      .map(_.split(" ").toSet)
      .toList
  }

}
