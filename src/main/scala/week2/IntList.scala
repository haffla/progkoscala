package week2

abstract class IntList {

  def isEmpty:Boolean
  def head:Int
  def tail:IntList
  def nth(index:Int):Int
  def contains(elem:Int):Boolean
  def insert(elem:Int):IntList
  def insertS(elem:Int):IntList
  def delete(elem:Int):IntList
  def deleteAll(elem:Int):IntList
	
  def insertSO(elem:Int):IntList = insertS(elem)
  
  def insertionSort:IntList =
    if(isEmpty) this
    else tail.insertionSort.insertSO(head)

  def map(func: Int => Int):IntList = this match {
    case Empty => this
    case Cons(head, tail) => Cons(func(head), tail.map(func))
  }

  def filter(func: Int => Boolean):IntList/* = this match {
    case Empty => this
    case Cons(head, tail) =>
      if(func(head)) Cons(head, tail.filter(func))
      else tail.filter(func)
  }*/

}