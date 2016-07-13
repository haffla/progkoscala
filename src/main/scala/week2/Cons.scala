package week2

case class Cons (head:Int, tail:IntList) extends IntList {

  def isEmpty=false
  
  def nth(index:Int):Int= index match{
    case 0 => head
    case i => tail.nth(i-1)
  }
  
  def contains(elem:Int):Boolean= elem match{
    case y if y==head => true
    case _ => tail.contains(elem)
  }

  def insert(X:Int):IntList= new Cons(X,this)
  	
  def insertS(elem:Int):IntList = {
    if(elem <= head) Cons(elem, this)
    else Cons(head, tail.insertS(elem))
  }

  def delete(elem:Int):IntList = {
    if(head == elem) tail
    else Cons(head, tail.delete(elem))
  }

  def deleteAll(elem:Int):IntList = {
    if(head == elem) tail.deleteAll(elem)
    else Cons(head, tail.deleteAll(elem))
  }

  def filter(func: (Int) => Boolean): IntList = {
    if(func(head)) Cons(head, tail.filter(func))
    else tail.filter(func)
  }

  
}