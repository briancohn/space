package bbdl.space
import breeze.linalg._
import breeze.numerics._
import breeze.math._
import breeze.optimize.linear.LinearProgram
import spire.math.Real
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
	def apply(A: DenseMatrix[Double], seed: Int) = {
		val NumRows = A.rows
		val NumCols = A.cols
		// Initiate an output vector (starting with zeros)
		var q_dir = DenseVector.zeros[Double](NumRows)
		for( i <- 0 to NumCols-1 ) {
			val rand = new Random(seed=seed)

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

object LowLevelSimplex{
  def apply(A_input: DenseMatrix[Double], b_input: DenseVector[Double], c_input: DenseVector[Double]): DenseVector[Double] = {
    val lp = new breeze.optimize.linear.LinearProgram()
    import breeze.util.JavaArrayOps

    val ColNum = A_input.cols //ColNum = the number of variables
    val xs = Array.fill(ColNum)(lp.Real()) //xs is the number of variables in an array. If x_0 to x_n, ColNum defines n
    val b = JavaArrayOps.dvDToArray(b_input)
    val RowNum = b.length
    val A = JavaArrayOps.dmDToArray2(A_input)
    val c = JavaArrayOps.dvDToArray(c_input) //the objective function

    var Constraints = new Array[lp.Constraint](RowNum)
    for (i <- 0 to RowNum-1) {
    Constraints(i) = (for( (x, a) <- xs zip A(i)) yield (x * a)).reduce(_ + _)  <= b(i)
    }
    val lpp = (
    (for( (x, a) <- xs zip c) yield (x * a)).reduce(_ + _)
    subjectTo( Constraints:_* )
    )
    val x = lp.maximize(lpp).result
    x
}
}

object GenStartingPoint{
	//@param A DenseMatrix[Double] of a linear constraint system
	//@return ABlock DenseMatrix[Double] with A, -A, zeros, zeros in the top left, bottom left, top right, and bottom right quadrants of a (2m,2n) matrix.
	def GenABlock(A: DenseMatrix[Double]): DenseMatrix[Double] = {
		val NumCols = A.cols
		val NumRows = A.rows
		val AMatricies = DenseMatrix.vertcat(A, -A)

		val ZeroMat = DenseMatrix.zeros[Double](NumRows,NumCols)
		val ZeroMatricies = DenseMatrix.vertcat(ZeroMat, ZeroMat)

		DenseMatrix.horzcat(AMatricies, ZeroMatricies)
	}
	//Generates a (2n,2n) sized square matrix, where all 
	//quadrants are eye, except for the top left corner, which is -eye.
	//@param n RowLen of an A Matrix.
	//@return M DenseMatrix[Double] with Q1 -eye(n), and all other quadrants eye(n)
	def GenEyeBlock(NumCols: Int):  DenseMatrix[Double] ={
		val Eye_n = DenseMatrix.eye[Double](NumCols)
		val TopEye = DenseMatrix.horzcat(-Eye_n, Eye_n)
		val BottomEye = DenseMatrix.horzcat(Eye_n, Eye_n)
		DenseMatrix.vertcat(TopEye, BottomEye)
	}
	//@param A The Linear constraints matrix to expand
	//@return AExpanded DenseMatrix[Double] expanded to full block form.
	def ExpandAMatrix(A: DenseMatrix[Double]): DenseMatrix[Double] = {
		DenseMatrix.vertcat(GenABlock(A), GenEyeBlock(A.cols))		
	}
	//@param b The linear programming inequality value, b
	//@param ColNum The linear programming constraints A.cols
	//@return Expandedb a DenseVector[Double] with length (4n), with b, -b, zeros(n), ones(n)
	def ExpandbVector(b: DenseVector[Double], ColNum: Int) = {
		val n = ColNum
		DenseVector.vertcat(b, -b, DenseVector.zeros[Double](n), DenseVector.ones[Double](n))
	}
	//@param A Linear programmming constraint matrix
	//@return c a set of solutions.
	def GencVector(A: DenseMatrix[Double]) = {
		val NumCols = A.cols
		DenseVector.vertcat(DenseVector.zeros[Double](NumCols),
							DenseVector.ones[Double](NumCols))
	}
	//@param A Linear programming constraints in a DenseMatrix[Double].
	//@param b Linear programming DenseVector[Double], strictly less than its companion row in A.
	//@return x DenseVector[Double] centrally-maximized solution for inner point.
	def apply(A: DenseMatrix[Double], b: DenseVector[Double]) = {
		val ColNum = A.cols
		val AExpanded = ExpandAMatrix(A)
		val bExpanded = ExpandbVector(b, A.cols)
		val c = GencVector(A)
		val x = LowLevelSimplex(AExpanded, bExpanded, c)
		x
	}
}