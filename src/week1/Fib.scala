package week1

object Fib {
  def main(args: Array[String]): Unit = {
    println(fib(100))
  }

  def fib(n:Int):BigInt = {
    def fibIter(fibs: List[BigInt], i:Int):List[BigInt] = {
      if(i == n) fibs
      else {
        // length is definitely at least 2
        fibIter((fibs.head + fibs.tail.head) :: fibs, i+1)
      }
    }
    fibIter(List(BigInt(0),BigInt(1)), 1).head
  }
}
