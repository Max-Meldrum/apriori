import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.{Failure, Success, Try}
import util.control.Breaks._


object Apriori extends App {
  type ItemSet = Set[String]
  case class Item(set: ItemSet, support: Int)
  type Transaction = List[ItemSet]
  type FrequentItemSets = List[Item]
  type AssociationRule = (ItemSet, ItemSet, Double)

  val data = "data/T10I4D100K.dat"
  val dataTwo = "data/smaller.dat"
  val slides = "data/slidesexample.dat"
  val support = 0.4
  val confidence = 0.75

  run(slides, support, confidence)

  def run(file: String, support: Double, confidence: Double): Unit = {
    getTransactions(file) match {
      case Success(set) => {
        time {
          println("Running with support: " + support + ", and confidence: " + confidence)
          val frequentItems = getFrequentItemSets(set, support)
          println("Frequent ItemSets:\n" + frequentItems)
          printRules(generateAssociationRules(frequentItems, confidence))
        }
      }
      case Failure(e) => println(e.getMessage)
    }
  }

  def printRules(rules: List[AssociationRule]): Unit = {
    println("Association rules:")
    rules.foreach {rule =>
      print("( ")
      rule._1.foreach(x => print(x + " "))
      print(") ---> ( ")
      rule._2.foreach(l => print(l + " "))
      print(") = ")
      print(rule._3 + "\n")
    }
  }

  def getTransactions(file: String): Try[Transaction] = Try {
    Source.fromFile(file)
      .getLines()
      .map(_.split(" ").toSet)
      .toList
  }

  def getFrequentItemSets(transaction: Transaction, minsup: Double): List[Item]  = {
    // Map singletons with frequency
    val frequencyMap = transaction.flatten.foldLeft(Map[String,Int]() withDefaultValue 0) {
      (m,x) => m + (x -> (1 + m(x)))
    }

    val transactionsNeeded = (transaction.size * minsup).toInt

    // Filter Singletons
    val currentSet = frequencyMap.filter(item => item._2 >= transactionsNeeded).toList
    val items = currentSet.map(tuple => Item(Set(tuple._1), tuple._2))
    // List(Item(Set(5), support), Item(Set(..), support)

    // Current tuple size value
    var k = 2
    val result = ListBuffer(items)

    // Make this more functional?
    breakable {
      while (true) {
        val nextItemSetK = items.flatMap(_.set)
          .combinations(k)
          .map(_.toSet)
          .toList

        val supportedK = nextItemSetK.map(set => Item(set, getSupport(set, transaction)))
          .filter(_.support >= transactionsNeeded)

        // Nothing more to process..
        if (supportedK.isEmpty)
          break

        // Add new ItemSets that are supported, and increase k-tuple size
        result += supportedK
        k = k + 1
      }
    }

    result.flatten
      .toList
  }

  def generateAssociationRules(items: FrequentItemSets, conf: Double): List[AssociationRule] = {
    // Just to have an easier way to access each sets support..
    val map = items.map(item => item.set -> item.support)
      .toMap

    val rules = items.map { item =>
      val set = item.set
      set.subsets().filter(x => (x.nonEmpty && x.size != set.size))
        .map {subset =>
          (subset, set diff subset, map(set).toDouble/map(subset).toDouble)
        }.toList
    }.flatten

    // Return only those confidence higher than "conf"
    rules.filter(rule => rule._3 >= conf)
  }

  def getSupport(set: ItemSet, transaction: Transaction): Int =
    transaction.count(line => set.subsetOf(line))

  // Cred to https://gist.github.com/mariussoutier/3293709
  def time[T](block: => T): T = {
    val start = System.currentTimeMillis
    val res = block
    val totalTime = System.currentTimeMillis - start
    println("Elapsed time: %1d ms".format(totalTime))
    res
  }


}
