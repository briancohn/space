package bbdl.space
import breeze.linalg._
import breeze.numerics._
import breeze.math._
import scala.util._


object MyAdder extends Function2[Double, Double, Double] {
	def apply(a: Double, b: Double) = {
		a + b
	}
}

// @return B DenseMatrix[Double] basis vectors matrix, with each column representing a basis vector.
object Basis extends Function1[DenseMatrix[Double], DenseMatrix[Double]]{
	def ExtractRightSquare(M: DenseMatrix[Double]) = {
		val RowNum = M.rows //m
		val ColNum = M.cols //n
		val StartCol = ColNum-RowNum
		val output = M(::, StartCol to -1)
		output
	}
	def InsertCol(M: DenseMatrix[Double], v: DenseVector[Double], k: Int) = {
		for( RowNum <- 0 to M.rows) {
			M(RowNum, k) = v(RowNum)
		}
		M
	}
	def apply(generators: DenseMatrix[Double]) = {
		val RowNum = generators.rows
		val ColNum = generators.cols
		var BasisSolutions = DenseMatrix.zeros[Double](RowNum, ColNum-RowNum)
		if (RowNum > ColNum)
    		println("You need to input a matrix where the number of columns is equal to or higher than the number of rows.")
		var CurrentColToInsert = 0
    //remember that this list is noninclusive;the second element is not in the list.
		val list = List.range(0, ColNum-RowNum)
		for(i  <- list) {
			val RightSquare = ExtractRightSquare(generators).toDenseMatrix
			val CurrentCol = generators(::,i).toDenseMatrix
			val x = RightSquare \ -CurrentCol.t
			val CurrentSolution = x.toDenseVector
//			BasisSolutions = InsertCol(BasisSolutions, CurrentSolution, CurrentColToInsert)
			BasisSolutions(::,i) := CurrentSolution
      CurrentColToInsert = CurrentColToInsert + 1

		}
		//generate a square matrix with ones down the middle, to represent each of the basis vectors we solved for.
		val DiagonalMat = DenseMatrix.eye[Double](ColNum-RowNum)
		DenseMatrix.vertcat(DiagonalMat,BasisSolutions)
	}
}
	


object Ortho extends Function[DenseMatrix[Double], DenseMatrix[Double]]{
	def apply(a: DenseMatrix[Double]) = {
		val m = a.rows
		val n = a.cols
		var BasisVector = a(::, 0)
		var b = DenseMatrix.create[Double](m, 1, (BasisVector / norm(BasisVector)).toArray)
		for (i <- 1 until n) {
		  BasisVector = a(::, i)
		  BasisVector = BasisVector - b * (b.t * BasisVector)
      //THIS LINE ADDED BY BRIAN COHN FRIDAY FEBRUARY 13,2015, 18:56PST
      BasisVector = BasisVector/norm(BasisVector)
      //END ADDITION
		  b = DenseMatrix.horzcat(b, DenseMatrix.create[Double](m, 1, BasisVector.toArray))
		}
		b
	} 
}

object GetRandomDirection{
	def apply(A: DenseMatrix[Double]) = {
		val NumRows = A.rows
		val NumCols = A.cols
		// Initiate an output vector (starting with zeros)
		var q_dir = DenseVector.zeros[Double](NumRows)
		for( i <- 0 to NumCols-1 ) {
			val rand = new Random(seed=1)
			val lambda = rand.nextGaussian()
			val x = A(::, i) :* lambda
			q_dir := x
		}
		q_dir
	}
}



object GetEndpoints{
  def apply(p: DenseVector[Double], q: DenseVector[Double]) = {
    val dimensions = p.length
    val fakeresult = DenseVector.ones[Double](dimensions)
    (fakeresult, fakeresult)
  }
}


//object IsOrthogonal{
//
//  def AreOrthogonal(v1: DenseVector[Double], v2: DenseVector[Double]) ={
//    val res = v1 dot v2
//    res
//  }
//  def apply(M: DenseMatrix[Double]) {
//    val combinations = (for {
//      i <- 1 to M.cols
//      j <- 1 until i
//      if j!=i
//    } yield (i,j))
//    val FalseList = combinations.filter((x: List[Int,Int]) AreOrthogonal(M(;;,x._1), M(;;,x._2)))
//    if (FalseList.length > 0) {
//      false
//
//    }else{
//      true
//    }
//     //if you get through all combinations and could only find orthogonal pairs
//  }
//}