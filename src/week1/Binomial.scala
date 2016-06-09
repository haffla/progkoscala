package week1

object Binomial {
  def main(args: Array[String]): Unit = {
    println(bino(0,2))
  }

  def bino(n:Int, k:Int): Int = {
    if(k == 0 || n == 0) 1
    else if(k == n) 1
    else
      bino(n-1, k-1) + bino(n-1, k)
  }
}
