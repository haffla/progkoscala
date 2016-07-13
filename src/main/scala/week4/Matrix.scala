package week4

class Matrix(val data:Array[Array[Double]]){

  type Vector= Array[Double]

  require (data != null && data.length > 0 && data(0).length > 0 && (data forall(x=> x.length == data(0).length)))

  def toArray:Array[Array[Double]] = data

  val columnLength = data.length
  val rowLength = data(0).length

  def equals(matrix:Matrix):Boolean = {
    data.flatMap(x=>x).sameElements(matrix.toArray flatMap(x=>x))
  }

  def transpose:Matrix= new Matrix(data.transpose)

  override def toString = {

    val s= for (row <- data) yield row mkString "\t"
    s mkString "\n"
  }

  // adds two matrices
  def +(matrix:Matrix):Matrix={
	  require (matrix.rowLength == rowLength && matrix.columnLength == columnLength)

    new Matrix(
      for {
        (r, mR) <- data zip matrix.data
      } yield (r zip mR) map { case (a, b ) => a + b}
    )
  }

  // multiplies a matrix with a constant
  def *(value:Double):Matrix = {
    new Matrix(
      for(d <- data) yield d.map(x => x * value)
    )
  }

  // multiplies each row with a vector and sums all components
  def *(v: Vector): Vector = {
    require(rowLength == v.length)
    (for(row <- data) yield (row zip v) map { case (a, b) => a * b }).map(x => x.sum)
  }

  // multiplies two matrices
  def *(matrix: Matrix):Matrix = {
    require(this.rowLength == matrix.columnLength)

    new Matrix(
      for(r <- data) yield for {
        mR <- matrix.transpose.data
      } yield ((r zip mR) map { case (a,b) => a * b}).sum
    )
  } 

  // transform a matrix in to a sparse representation
  def toSparseMatrix:SparseMatrix = {
    val ar = for {
      (row, rowIndex) <- data.zipWithIndex
      (v, colIndex) <- row.zipWithIndex
      if v != 0
    } yield ((rowIndex, colIndex), v)
    new SparseMatrix(ar.toList, rowLength, columnLength)
  }
}

object Matrix{

  type Vector= Array[Double]
  // creates a matrix from a sparse representation
  def createFromSparse(l:List[((Int,Int),Double)], rowLength:Int, colLength:Int):Matrix={
    val sparseData:Array[Array[Double]] = Array.fill(rowLength){
      Array.fill(colLength){0}
    }
    l.foreach(x => sparseData(x._1._1)(x._1._2) = x._2)
    new Matrix(sparseData)
  }
}