package bbdl.space

import java.io.File
import java.util.Date

import breeze.linalg._
import breeze.numerics._
import breeze.math._
import breeze.optimize.linear.LinearProgram
import spire.math.Real
import scala.util._

/*
Set of functions for generating upper and lower bounds of each lambda from i to A.cols (n).
 */
object Bounds{
  /*
  Takes in an A Matrix, and connects a negative copy below it
   */
  def GenABlock(A: DenseMatrix[Double]) = {
    DenseMatrix.vertcat(A, -A)
  }
  /*
  Takes in the col num (Int) for the A matrix, and outputs a negative eye and positive eye (vertically concatenated)
   */
  def GenEyeBlock(n: Int) = {
    val eye = DenseMatrix.eye[Double](n)
    DenseMatrix.vertcat(-eye,eye)
  }
  /*
  Takes in the A DenseMatrix and outputs A, -A, -eye, eye. - all vertically concatenated.
   */
  def ExpandedMatrix(A: DenseMatrix[Double]) = {
    DenseMatrix.vertcat(GenABlock(A), GenEyeBlock(A.cols))
  }
  /*
  Takes in the cols of A, and the b vector to generate v, -v, zeros(A.cols), ones(A.cols) to set the constraints for each col.
   */
  def ExpandedVector(ACols: Int, b: DenseVector[Double]) = {
    DenseVector.vertcat(b,-b,DenseVector.zeros[Double](ACols),DenseVector.ones[Double](ACols))

  }
  /*
  @param Number an integer which will be inserted among zeros
  @param len the length of the vector to return
  @param NumberIndex the index of the vector which you want equal to the number
   */
  def NumberAmongZeros(Number: Double, len: Int, NumberIndex: Int): DenseVector[Double] = {
    var v = DenseVector.zeros[Double](len)
    v(NumberIndex) = Number
    v
  }
  /*
  Extracts one upper or lower bound from the system of A and b.
  @param Col The lambda you want to get an upperbound or lowerbound from (in Double format)
   */
  def ColBound(A: DenseMatrix[Double], b: DenseVector[Double], Col: Int, UpperOrLower: String): Double = {
    val c = NumberAmongZeros(1.0,A.cols,Col)
    val A_new = ExpandedMatrix(A)
    val b_new = ExpandedVector(A.cols, b)
    if (UpperOrLower == "Upper") {
      LowLevelSimplex(A_new,b_new,  c)(Col)
    } else {
      LowLevelSimplex(A_new,b_new, -c)(Col)
    }
  }
  /*
  Takes in an A matrix and a b vector (representing an Ax <=b linear system) and outputs the upperbounds for each column of A [each lambda].)
   */
  def ComputeUppers(A: DenseMatrix[Double], b: DenseVector[Double]) = {
    var res = DenseVector.zeros[Double](A.cols)
    for (i <- 0 to A.cols-1) {
      res(i) = ColBound(A,b,i, "Upper")
    }
    res
  }
  /*
Takes in an A matrix and a b vector (representing an Ax <=b linear system) and outputs the lowerbounds for each column of A [each lambda].)
 */

  def ComputeLowers(A: DenseMatrix[Double], b: DenseVector[Double]) = {
    var res = DenseVector.zeros[Double](A.cols)
    for (i <- 0 to A.cols-1) {
      res(i) = ColBound(A,b,i, "Lower")
    }
    res
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
		val AWithoutEpsilonBounds = DenseMatrix.vertcat(GenABlock(A), GenEyeBlock(A.cols))
    val n = AWithoutEpsilonBounds.cols/2
    val EpsilonBounds = DenseMatrix.horzcat(DenseMatrix.zeros[Double](n, n), -DenseMatrix.eye[Double](n))
    DenseMatrix.vertcat(AWithoutEpsilonBounds, EpsilonBounds)
	}
	//@param b The linear programming inequality value, b
	//@param ColNum The linear programming constraints A.cols
	//@return Expandedb a DenseVector[Double] with length (4n), with b, -b, zeros(n), ones(n)
	def ExpandbVector(b: DenseVector[Double], ColNum: Int) = {
		val n = ColNum
		DenseVector.vertcat(b, -b, DenseVector.zeros[Double](n), DenseVector.ones[Double](n), DenseVector.zeros[Double](n))
	}
	//@param A Linear programmming constraint matrix
	//@return c a set of solutions.
	def GencVector(A: DenseMatrix[Double]) = {
		val NumCols = A.cols
		DenseVector.vertcat(DenseVector.zeros[Double](NumCols),
							DenseVector.ones[Double](NumCols))
	}

	/** Generation of a central starting point within the solution space.
  		* @param A Linear programming constraints in a DenseMatrix[Double]. 
  		* @param b Linear programming DenseVector[Double], strictly less than its companion row in A.
  		* @return x DenseVector[Double] centrally-maximized solution for inner point.
  	*/
	def apply(A: DenseMatrix[Double], b: DenseVector[Double]) = {
		val ColNum = A.cols
		val AExpanded = ExpandAMatrix(A)
		val bExpanded = ExpandbVector(b, A.cols)
		val c = GencVector(A)
		val x = LowLevelSimplex(AExpanded, bExpanded, c)
		x(0 to ColNum-1)
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

//TODO Write more tests for this
/*
*by default the gaussian distribution is set to mean 0, sd 1
 */
object GetRandomDirection{

	def apply(A: DenseMatrix[Double], RandomObject: scala.util.Random) = {
    var Lambdas = DenseVector.zeros[Double](A.cols)
    for (i <- 0 to A.cols - 1){
      Lambdas(i) = RandomObject.nextGaussian()
    }
//    println(Lambdas)
   	val LambdaVec = DenseVector(Lambdas.toArray)
		A * LambdaVec //matrix multiplication means that it multiplies and adds all the rows up.
	}
	// def apply(A: DenseMatrix[Double], seed: Int) = {
	// 	val NumRows = A.rows
	// 	val NumCols = A.cols
	// 	// Initiate an output vector (starting with zeros)
	// 	var x = DenseVector.zeros[Double](NumRows)
	// 	for( i <- 0 to NumCols-1 ) {
	// 		var rand = new Random(seed=seed)
	// 		var lambda = rand.nextGaussian()
	// 		x = x + A(::, i) :* lambda
	// 	}
	// 	x
	// }
}


//@param p The point cordinate, DenseVector[Double] of length n
//@param q The direction, DenseVector[Double] of length n
object GetNewPoint{
  //@TODO Describe what each case means mathematically
  def LowerboundVal(p_val: Double, q_val: Double) = {
  	if(q_val > 0){
  		-(p_val/q_val)
  	} else if (q_val < 0){
  		(1-p_val)/q_val
  	} else {
  		-Inf
  	}
  }
  //@TODO Describe what each case means mathematically
  def UpperboundVal(p_val: Double, q_val: Double) = {
  	if(q_val > 0 ){
  		(1-p_val)/q_val
  	} else if (q_val < 0) {
  		-(p_val/q_val)
  	} else {
  		Inf
  	}
  }
  def GetUpperBoundVector(p: DenseVector[Double], q: DenseVector[Double]): DenseVector[Double]={
    val len = p.length
    var UpperBounds = DenseVector.ones[Double](len)*(Inf)
    for (i <- 0 to len-1) {
      UpperBounds(i) = UpperboundVal(p(i),q(i))
    }
    UpperBounds
  }
  def GetLowerBoundVector(p: DenseVector[Double], q: DenseVector[Double]): DenseVector[Double]={
    val len = p.length
    var LowerBounds = DenseVector.ones[Double](len)*(Inf)
    for (i <- 0 to len-1) {
      LowerBounds(i) = LowerboundVal(p(i),q(i))
    }
    LowerBounds
  }
  def GetBoundLimits(UpperBounds: DenseVector[Double], LowerBounds: DenseVector[Double]): Tuple2[Double,Double]= {
    val MinOfUpperBounds = UpperBounds.min
    val MaxOfLowerBounds = LowerBounds.max
    Tuple2(MinOfUpperBounds, MaxOfLowerBounds)
  }
  //@param pq a Tuple of two doubles, first is the point, second is the direction value for a given dimension.
  //@return upperbound value for that dimension
  def UpperboundValTuple(pq: Tuple2[Double, Double]): Double={
  	UpperboundVal(pq._1, pq._2)
  }
  //@param p DenseVector[Double], The starting point of length n
  //@param p  DenseVector[Double], The direction, of length n
  //@param UpperBoundInner Double, The min of the upper bound
  //@param LowerBoundInner Double, The max of the lower bound
  def FindEndpoints(p: DenseVector[Double], q: DenseVector[Double], UpperBoundInner: Double, LowerBoundInner:Double) = {
  	val FirstEndpoint = p + q*LowerBoundInner
  	val SecondEndpoint = p + q*UpperBoundInner
  	(FirstEndpoint, SecondEndpoint)
  }
  def GetEndpoints(p:DenseVector[Double], q: DenseVector[Double]) = {
    val Bounds = GetBoundLimits(GetUpperBoundVector(p,q), GetLowerBoundVector(p,q))
    FindEndpoints(p,q, Bounds._1, Bounds._2)
  }
  //Get New Point from p and q
  //@param p densevector of the point
  //@param q Densevector of the random direction
  def apply(p: DenseVector[Double], q: DenseVector[Double], RandomObject: scala.util.Random) = {
	  val Bounds = GetBoundLimits(GetUpperBoundVector(p,q), GetLowerBoundVector(p,q))
	  val Points = FindEndpoints(p,q, Bounds._1, Bounds._2)
      val res = RandomPointBetween(Points._1, Points._2, RandomObject)
      res
  }
}
//@param E1 Vector of coordinates for the second point
//@param E2 Vector of coordinates for the second point
//@param seed Int defining the random number seed for point generation.
object RandomPointBetween {
	def apply(E1: DenseVector[Double], E2: DenseVector[Double], RandomObject: scala.util.Random) = {
		val lambda = RandomObject.nextDouble() //between 0 and 1 by default
//		println(lambda)
    E1 + (E2-E1)*lambda
	}
}


//Just matrices for now
object ElementwiseAbsoluteDifference{
  def apply(A: DenseMatrix[Double], B: DenseMatrix[Double]): Double = {
    if (A.cols == B.cols && A.rows == A.rows) {
      var AbsDiff = 0.0
      for (col <- 0 to A.cols-1){
        for (row <- 0 to A.rows-1){
          AbsDiff = AbsDiff + abs(A(row,col) - B(row, col))
        }
      }
      AbsDiff
    } else {
      99999999999.0 //TODO change this to an error
    }
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


 object HitAndRun {
 	def apply(OrthonormalBasis: DenseMatrix[Double], StartingPoint: DenseVector[Double], RandomObject: scala.util.Random) = {
 		val RandomDirection = GetRandomDirection(OrthonormalBasis, RandomObject) //has a random step in gaussian distribution
    val NewPoint = GetNewPoint(StartingPoint, RandomDirection, RandomObject) //has a random step in uniform distribution
    NewPoint
  }
 }

object SampleLinearSystem {
  def apply(A: DenseMatrix[Double], v: DenseVector[Double], RandomObject: scala.util.Random, Samples: Int) = {
    val OrthonormalBasis = Ortho(Basis(A)) //Orthogonalize the basis
    var CurrentPoint = GenStartingPoint(A, v)
    val Seed = 10
    val RandomObject = new scala.util.Random(Seed)
    var PointDatabase = DenseMatrix.zeros[Double](Samples, A.cols)
    var RunningMean = CurrentPoint(0)
    for (i <- 0 to Samples - 1) {
      RunningMean = UpdateMean(CurrentPoint(0), RunningMean, i.toDouble + 2.0)
      //      println(RunningMean)
      //      (i.toDouble*0.2).toInt
      CurrentPoint = HitAndRun(OrthonormalBasis, CurrentPoint, RandomObject)
      PointDatabase(i, ::) := CurrentPoint.t
      //TODO add points into the database
    }
    PointDatabase

  }
}
object UpdateMean{
  def apply(NewValue: Double, PriorMean: Double, n: Double): Double = {
    val NConstant = (n-1.0)/n
    val WeightedNewValue = (NewValue/n)
    val PriorWeight = NConstant*PriorMean
    PriorWeight + WeightedNewValue
  }
}
/*

 */
object RangeExcursion{
  def ValueOutsideRange(v: DenseVector[Double], range: Double):  Boolean ={
    max(v) - min(v) > range
  }
}
object Output {
  def TimestampCSVName(prefix: String): String = {
    var timestamp = (System.currentTimeMillis).toString()
    var dateStr = prefix + timestamp + ".csv"
    dateStr
  }
}
object PointStream {
  //returns true when it's time to stop
  /*
  @param BeginIndex It will only compute the means once this number of rows have been sampled.
   */
  def MeanChaser(acc: DenseMatrix[Double]): Boolean ={
    val PointNum = acc.rows
    val BeginIndex=10000
    if (PointNum<BeginIndex) {
      if (PointNum%1000 == 0) println("False at " + PointNum)
      false

    } else {
//      val SampleDouble = PointNum*0.20
      val SampleNum = 2000
      val subset = acc(PointNum - SampleNum to PointNum-1, ::).toDenseMatrix.data
      false
    }
  }
  /*
  Never stops it
   */
  def AlwaysTrue(acc: DenseMatrix[Double]): Boolean = {
    true
  }
  /*
  This stops it once it samples 1m points.
   */
  def HardCodedStop(acc:DenseMatrix[Double]): Boolean = {
    if (acc.rows >= 1000000) {
      true //stop!
    } else {
      false //keep going
    }
  }


  def generator(OrthonormalBasis: DenseMatrix[Double], CurrentPoint: DenseVector[Double], RandomObject: scala.util.Random): DenseVector[Double] = {
    HitAndRun(OrthonormalBasis,CurrentPoint,RandomObject)
  }
  def fill(OrthonormalBasis: DenseMatrix[Double], CurrentPoint: DenseVector[Double], RandomObject: scala.util.Random, Predicate: DenseMatrix[Double]=>Boolean, acc: DenseMatrix[Double]):DenseMatrix[Double] ={
    if (Predicate(acc)){
      println("stop")
      acc
    } else {
        val NewPt = generator(OrthonormalBasis, CurrentPoint, RandomObject)
        if (acc.rows == 10000) {
          val FileName = Output.TimestampCSVName("output/").toString()
          val MyFile = new java.io.File(FileName)
          println("Saving to " + FileName)
          csvwrite(MyFile, acc)
        fill(OrthonormalBasis, NewPt, RandomObject, Predicate, NewPt.toDenseMatrix) //start over
        } else {
        fill(OrthonormalBasis, NewPt, RandomObject, Predicate, DenseMatrix.vertcat(acc, NewPt.toDenseMatrix))
      }
    }
  }
  def start(OrthonormalBasis: DenseMatrix[Double], StartingPoint: DenseVector[Double], RandomObject: scala.util.Random, Predicate: DenseMatrix[Double]=>Boolean): Unit = {
    fill(OrthonormalBasis, StartingPoint, RandomObject, Predicate, StartingPoint.toDenseMatrix)
  }

}

