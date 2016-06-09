package week1

object Parans {

  def main(args: Array[String]):Unit = {
    println(check("(kleiner) ((()Test))"))
    println(check(")(()"))
  }

  def check(s: String):Boolean = {
    def iter(charList:List[Char], counter: Int): Boolean = {
      if(counter < 0) false
      else {
        charList match {
          case Nil => counter == 0
          case head :: tail =>
            if (head == '(') iter(tail, counter + 1)
            else if (head == ')') iter(tail, counter - 1)
            else iter(tail, counter)
        }
      }

    }

    iter(s.toCharArray.toList, 0)
  }

}
