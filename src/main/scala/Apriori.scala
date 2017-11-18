import scala.io.Source
import scala.util.{Failure, Success, Try}

object Apriori extends App {
  type ItemSet = Set[String]
  type Transaction = List[ItemSet]

  //val data = "data/T10I4D100K.dat"
  val data = "data/smaller.dat"
  run(data, 0.4)

  case class Item(set: ItemSet, support: Int)

  def run(file: String, support: Double): Unit = {
    getItemSet(data) match {
      case Success(set) => time(getFrequentItems(set, support))
      case Failure(e) => println(e.getMessage)
    }
  }

  def getItemSet(file: String): Try[Transaction] = Try {
    Source.fromFile(file)
      .getLines()
      .map(_.split(" ").toSet)
      .toList
  }

  def getFrequentItems(transaction: Transaction, minsup: Double): Transaction = {
    val frequencyMap = transaction.flatten.foldLeft(Map[String,Int]() withDefaultValue 0) {
      (m,x) => m + (x -> (1 + m(x)))
    }
    val transactionsNeeded = (transaction.size * minsup).toInt
    println(transactionsNeeded)

    // Filter items that has support into a new set
    val currentSet= frequencyMap.filter(item => item._2 >= transactionsNeeded).toList
    val items = currentSet.map(tuple => Item(Set(tuple._1), tuple._2))
    // List(Item(Set(5), support), Item(Set(..), support)

    // Current tuple size value
    var k = 2

    val nextItemsetC = items.flatMap(_.set)
      .combinations(k)
      .map(_.toSet)
      .toList

    println(nextItemsetC)
    val supportedC2 = nextItemsetC.map(set => Item(set, getSupport(set, transaction)))
    println(supportedC2)

    transaction
  }


  // Cred to https://gist.github.com/mariussoutier/3293709
  def time[T](block: => T): T = {
    val start = System.currentTimeMillis
    val res = block
    val totalTime = System.currentTimeMillis - start
    println("Elapsed time: %1d ms".format(totalTime))
    res
  }

  def getSupport(set: ItemSet, transaction: Transaction): Int =
    transaction.count(line => set.subsetOf(line))

}
