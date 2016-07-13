package week2

case object Empty extends IntList{
  def isEmpty=true
  def head:Int= throw new Error("head.nil")
  def tail:IntList= throw new Error("tail.nil")
  def contains(elem:Int):Boolean=false
  def nth(index:Int)= throw new Error("IndexOutOfBound")
  def insert(X:Int):IntList= Cons(X,this)
  def insertS(elem:Int):IntList= insert(elem)
  def delete(elem:Int):IntList= this
  def deleteAll(elem:Int):IntList = this
  def filter(func: (Int) => Boolean): IntList = this
}