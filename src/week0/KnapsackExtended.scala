package week0

object KnapsackExtended {

  val maxWeight = 15

  val items = List(
    Item(1, 10),
    Item(2, 20),
    Item(5, 5),
    Item(9, 5),
    Item(15, 100)
  )

  case class Item(weight: Int, value: Int) {
    override def toString = "[Weight: " + weight + ", Value: " + value + "]"
  }

  def main(args: Array[String]) {

    getSolution(Knapsack.knapsack(items), maxWeight) match {
      case Some(sol) => println("Solution: " + sol)
      case _ => println("No solution!")
    }
  }

  def getSolution(items: List[List[Item]], maxWeight: Int): Option[List[Item]] = {
    items.filter(fitsInKnapsack).sortWith(compareTotalValue).headOption
  }

  private def compareTotalValue(a:List[Item], b:List[Item]): Boolean = {
    a.foldLeft(0)( (l, r) => l + r.value ) > b.foldLeft(0)( (l, r) => l + r.value )
  }

  private def fitsInKnapsack(items: List[Item]): Boolean = {
    items.foldLeft(0)( (l, r) => l + r.weight ) <= maxWeight
  }

}
