package week5

import scala.annotation.tailrec

object BasicOperations {

  	def mapper[KeyIn, ValueIn, KeyMOut, ValueMOut](mapFun: ((KeyIn, ValueIn)) => List[(KeyMOut, ValueMOut)],
											 															data:List[(KeyIn, ValueIn)]):List[(KeyMOut, ValueMOut)] = {
			data flatMap mapFun
		}
  	
  	def sorter[KeyMOut, ValueMOut](data:List[(KeyMOut,ValueMOut)]): List[(KeyMOut, List[ValueMOut])] = {
			( data.groupBy(_._1) mapValues(x => x.map(_._2)) ).toList
		}
  	
  	def reducer[KeyMOut, ValueMOut, KeyROut, ValueROut](redFun:((KeyMOut, List[ValueMOut])) => List[(KeyROut, ValueROut)],
																										data:List[(KeyMOut, List[ValueMOut])]): List[(KeyROut, ValueROut)] = {
			data flatMap redFun
		}
  	
  	def mapReduce[KeyIn, ValueIn, KeyMOut, ValueMOut, KeyROut, ValueROut](mapFun: ((KeyIn, ValueIn)) => List[(KeyMOut, ValueMOut)],
					redFun: ( (KeyMOut, List[ValueMOut]) ) => List[(KeyROut, ValueROut)], data:List[(KeyIn, ValueIn)]): List[(KeyROut, ValueROut)] = {

      val mapped = mapper(mapFun, data)
      val sorted = sorter(mapped)
      reducer(redFun, sorted)
    }
  	
  	/* 
  	 * Wandeln Sie das WordCount-Beispiel aus der Vorlesung in die Map-Reduce-Variante um.
  	 * Die Funktion soll wie unten aufgefuehrt aufgerufen werden koennen.
  	 * 
  	 * */
	def wordCount(text: List[(Int, String)]): List[(String,Int)] = {
    mapReduce[Int, String, String, Int, String, Int](
      x => {
        val text = x._2.toLowerCase.replaceAll("[^a-z]", " ")
        text.split(" ").toList.map(x => (x, 1)).filter(x => x._1 != "")
      },
      x => List( (x._1, x._2.sum) ),
      text
    )
  }


	/*
	 * Schreiben Sie eine Funktion, die fuer eine Liste von Zahlen, jeweils die Prim-Teil 
	 * berechnet und aufsummiert.
	 * 
	 * So hat bspw. die 24 folgende Prim-Teiler 2,2,2,3 die Summe davon ist 9.
	 * Wenden Sie die Funktion mit MapReduce an. Ergebnis soll eine Liste von Tupeln sein,
	 * die an erster Stelle die Zahl enthaelt und an zweiter Stelle die Summe der Primteiler.
	 * 
	 */
  @tailrec
	def primf(x:Int, teiler:Int = 2, list: List[Int] = Nil):List[Int] = teiler * teiler > x match {
    case false if x % teiler == 0 => primf(x / teiler, teiler, teiler :: list)
    case false                    => primf(x, teiler + 1, list)
    case true                     => x :: list
  }
	
	
	def primTeilerSumme(l:List[Int]):List[(Int, Int)]= {
    mapReduce[Int, Int, Int, Int, Int, Int](
      x => (x._2, primf(x._2).sum) :: Nil,
      x => (x._1, x._2.headOption.getOrElse(0)) :: Nil,
      l.zipWithIndex map(_.swap)
    )
  }
	
	/* Schreiben Sie eine Funktion, die für eine Liste von Wörtern alle Anagramme findet
	 * Benutzen Sie dafür die MapReduce-Funktion
	 */
	
	def findAnagrams(l:List[String]):List[(String,String)] = {
    mapReduce[Int, String, String, String, String, String](
      x => List( (x._2.sorted, x._2) ),
      x => x._2 match {
        case first :: second :: Nil => List( (first, second) )
        case _ => Nil
      },
      l.zipWithIndex map(_.swap)
    )
  }

  def anagrams(word: String): List[String] = {

    def stringCharCombinations(s: String, c: Char): Seq[String] =
      for(i <- 0 to s.length) yield {
        s.substring(0, i) + c + s.substring(i, s.length)
      }

    def makeStep(w: String): List[String] = {
      if(w.length == 0) w :: Nil
      else {
        val x = makeStep(w.substring(0, w.length - 1))
        x flatMap { p =>
          stringCharCombinations(p, w.charAt(w.length - 1))
        }
      }
    }
    makeStep(word)

  }

  def main(args: Array[String]): Unit = {

    println(wordCount(List((0, "Dies ist ein Test"),(1, "und jetzt kommt noch ein Test!"), (2, "mal schauen, ob es funktioniert"))))
    println(primf(24))
    println(primTeilerSumme(List(12,24,8,36)))
    println(findAnagrams(List("otto","toto","hans","haus","heute","geist","huete","siegt")))

  }
}
