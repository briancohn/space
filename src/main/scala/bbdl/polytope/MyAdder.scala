package bbdl.polytope
import breeze.linalg._
import breeze.numerics._
import scala.util.Random._


class MyAdder extends Function2[Double, Double, Double] {
	def apply(a: Double, b: Double) = {
		a + b
	}
}

class GetRandomDirection {
	def apply(B: DenseMatrix[Double], v: DenseVector[Double]) = {
		0
	}

}

// @return B DenseMatrix[Double] basis vectors matrix, with each column representing a basis vector.
class Basis{
	def ExtractRightSquare(M: DenseMatrix[Double]) = {
		val RowNum = M.rows //m
		val ColNum = M.cols //n
		val StartCol = ColNum-RowNum
		var output = M(::, StartCol to -1)
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
		val list = List.range(0, ColNum-RowNum-1) 
		for(i  <- list) {
			val RightSquare = ExtractRightSquare(generators).toDenseMatrix
			val CurrentCol = generators(::,i).toDenseMatrix
			val x = RightSquare \ -CurrentCol
			val CurrentSolution = x.toDenseVector(1 to -1) //exclude the first element, which we know is a one.
			BasisSolutions = InsertCol(BasisSolutions, CurrentSolution, CurrentColToInsert)
			CurrentColToInsert = CurrentColToInsert + 1
		}
		//generate a square matrix with ones down the middle, to represent each of the basis vectors we solved for.
		val DiagonalMat = DenseMatrix.eye[Double](ColNum-RowNum)
		DenseMatrix.vertcat(DiagonalMat,BasisSolutions)
	}
}
	


class Ortho {
	def apply(a: DenseMatrix[Double]) = {
		val m = a.rows
		val n = a.cols
		var BasisVector = a(::, 0)
		var b = DenseMatrix.create[Double](m, 1, (BasisVector / norm(BasisVector)).toArray)
		for (i <- 1 until n) {
		  BasisVector = a(::, i)
		  BasisVector = BasisVector - b * (b.t * BasisVector)
		  b = DenseMatrix.horzcat(b, DenseMatrix.create[Double](m, 1, BasisVector.toArray))
		}
		b
	} 
}

class GetRandomDirection{
	def apply(A: DenseMatrix[Double]) = {
		NumRows = A.rows
		NumCols = A.cols
		for( i <- 0 to NumCols-1 ) {
			val rand = new Random(seed=1)
			val lambda = nextGaussian()
			val x = lambda * A(::, i)
			val q = q + x
		}
		q
	}
}
