package week4

class SparseMatrix (val data_list:List[((Int,Int),Double)], val rowLength:Int, val colLength:Int) {

  type Vector = Array[Double]
  type TupleGetFunc = ((Int, Int)) => Int

  val data = data_list.toMap
  def toList = data.toList

  def *(v:Vector):Vector = ???

  def *(matrix:SparseMatrix):SparseMatrix = ???

  def equals(matrix:SparseMatrix):Boolean =
    data_list.filter(notZero).toSet == matrix.data_list.filter(notZero).toSet

  def notZero(v: ((Int, Int), Double)): Boolean = v._2 != 0

  def toMatrix: Matrix = Matrix.createFromSparse(data.toList, rowLength, colLength)

  def getRow(row_nr:Int):Vector = {
    val arr: Array[Double] = Array.fill(rowLength){0}
    getRespectiveVals(row_nr, getRowFromTuple, getColFromTuple).foreach( x => arr(x._1) = x._2)
    arr
  }

  def getColumn(col_nr:Int):Vector = {
    val arr: Array[Double] = Array.fill(colLength){0}
    getRespectiveVals(col_nr, getColFromTuple, getRowFromTuple).foreach( x => arr(x._1) = x._2)
    arr
  }

  private def getRespectiveVals(nr: Int, funcOne: TupleGetFunc, funcTwo: TupleGetFunc): List[(Int, Double)] =
    data_list.filter( e => funcOne(e._1) == nr ).map( x => (funcTwo(x._1), x._2))

  private def getRowFromTuple(tuple: (Int, Int)):Int = tuple._1
  private def getColFromTuple(tuple: (Int, Int)):Int = tuple._2
}