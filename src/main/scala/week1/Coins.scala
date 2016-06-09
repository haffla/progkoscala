package week1

object Coins {

    def main(args: Array[String]):Unit = {
      println(calcCoinDenomsList(List(2,1), 4))
    }

    def calcCoinDenoms(werte: List[Int], betrag: Int): Int = {
      if(betrag < 0) 0
      else if(werte.isEmpty) {
        if(betrag == 0) 1 else 0
      }
      else {
        calcCoinDenoms(werte, betrag - werte.head) +
          calcCoinDenoms(werte.tail, betrag)
      }
    }

    def calcCoinDenomsList(werte: List[Int], betrag: Int): List[List[Int]] = {

      def doCalc(coins: List[Int], amount: Int, current: List[Int]): List[List[Int]] = {
        if(amount < 0) Nil
        else if(coins.isEmpty) {
          if(amount == 0) List(current) else Nil
        }
        else {
          doCalc(coins, amount - coins.head, coins.head::current) :::
            doCalc(coins.tail, amount, current)
        }
      }

      doCalc(werte, betrag, Nil)
    }
}
