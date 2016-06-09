package week1

object Prime {
  def main(args: Array[String]): Unit = {
    println(ithPrime(10001) == 104743)
  }

  def ithPrime(i:Int): Int =
    if (i < 1) throw new Exception("i must be greater than or equal to 1")
    else
      allPrimes.take(i).last

  def allPrimes:Stream[Int] = Stream.from(2) filter isPrime

  def isPrime(i:Int): Boolean = 2 until i forall(i % _ != 0)
}