object Knapsack {

  def main(args: Array[String]) {
    val res = knapsack(List(1,2,5,9))
    res.foreach(println(_))
    getSolution(res, 16) match {
      case Some(s) => println("Solution: " + s)
      case _ => println("No Solution!")
    }
  }

  def knapsack[T](numbers: List[T]): List[List[T]] = {
    numbers match {
      case Nil => List(Nil)
      case head ::  Nil => List(head) :: List(Nil)
      case head :: tail =>
        val tailSack = knapsack(tail)
        tailSack ++ tailSack.map(l => head :: l)
    }
  }

  def getSolution[T:Numeric](solutions: List[List[T]], sum: T): Option[List[T]] = solutions.find(_.sum == sum)
}
