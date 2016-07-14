package week4

class SparseMatrix (val data_list:List[((Int,Int),Double)], val nrRows:Int, val nrCols:Int) {

  type Vector = Array[Double]
  type TupleGetFunc = ((Int, Int)) => Int

  val data = data_list.toMap
  def toList = data.toList

  val colLength = nrRows
  val rowLength = nrCols

  def *(v:Vector):Vector = {
    require(v.length == nrRows)

    for(i <- (0 until nrCols).toArray)
      yield ( (v zip getColumn(i)) map { case (a, b) => a*b} ).sum
  }

  def *(matrix:SparseMatrix):SparseMatrix = {
    val dataSeq = for(rowIndex <- 0 until nrRows; colIndex <- 0 until matrix.nrCols) yield {
      val row = getRow(rowIndex)
      val col = matrix.getColumn(colIndex)
      val res = (row zip col) map { case (a,b) => a * b}
      ((rowIndex, colIndex), res.sum)
    }

    new SparseMatrix(dataSeq.toList, nrRows, matrix.nrCols)
  }

  def equals(matrix:SparseMatrix):Boolean =
    data_list.filter(notZero).toSet == matrix.data_list.filter(notZero).toSet

  def notZero(v: ((Int, Int), Double)): Boolean = v._2 != 0

  def toMatrix: Matrix = Matrix.createFromSparse(data.toList, nrRows, nrCols)

  def getRow(row_nr:Int):Vector = {
    val valueMap = getRespectiveVals(row_nr, getRowFromTuple, getColFromTuple)
    (0 until rowLength).toArray.map { index => valueMap.getOrElse(index, 0.0)}
  }

  def getColumn(col_nr:Int):Vector = {
    val valueMap = getRespectiveVals(col_nr, getColFromTuple, getRowFromTuple)
    (0 until colLength).toArray.map { index => valueMap.getOrElse(index, 0.0)}
  }

  private def getRespectiveVals(nr: Int, funcOne: TupleGetFunc, funcTwo: TupleGetFunc): Map[Int, Double] =
    data_list.filter( e => funcOne(e._1) == nr ).foldLeft(Map[Int, Double]()) { case (acc, (pos, value)) =>
      acc + (funcTwo(pos) -> value)
    }

  private def getRowFromTuple(tuple: (Int, Int)):Int = tuple._1
  private def getColFromTuple(tuple: (Int, Int)):Int = tuple._2
}