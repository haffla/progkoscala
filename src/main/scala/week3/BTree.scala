package week3

abstract class BTree {
  def contains(elem: Int): Boolean
  def insert(elem: Int): BTree
  def merge(other: BTree): BTree
}

case class Empty() extends BTree {
  def contains(elem: Int): Boolean = false
  def insert(elem: Int): BTree = NonEmpty(Empty(), elem, Empty())
  def merge(other: BTree): BTree = other
}

case class NonEmpty(left:BTree, elem: Int, right: BTree) extends BTree {

  def contains(elem: Int): Boolean = {
    if(elem == this.elem) true
    else if(elem < this.elem) left contains elem
    else right contains elem
  }

  def insert(elem: Int): BTree = {
    if(elem == this.elem) this
    else if(elem < this.elem) NonEmpty(left insert elem, this.elem, right)
    else new NonEmpty(left, this.elem, right insert elem)
  }

  def merge(other: BTree): BTree =
    ((left merge right) merge other) insert elem
}