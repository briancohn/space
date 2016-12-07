package bbdl.space

import breeze.linalg.{DenseVector, DenseMatrix}

/**
  * Created by olive on 11/4/16.
  */
/*
Set of functions for generating upper and lower bounds of each lambda from i to A.cols (n).
 */
object MatrixBoundsHelper {

  def GenABlock(x: LimbForceCapabilityMatrix): DenseMatrix[Double] = GenABlock(x.forceMatrix)

  /* Takes in an A Matrix, and connects a negative copy below it */
  def GenABlock(A: DenseMatrix[Double]) = DenseMatrix.vertcat(A, -A)

  /* Takes in the col num (Int) for the A matrix, and outputs a negative eye and positive eye (vertically concatenated) */
  def GenEyeBlock(n: Int) = {
    val eye = DenseMatrix.eye[Double](n)
    DenseMatrix.vertcat(-eye, eye)
  }

  /* Takes in the A DenseMatrix and outputs A, -A, -eye, eye. - all vertically concatenated. */
  def ExpandedMatrix(A: DenseMatrix[Double]) = DenseMatrix.vertcat(GenABlock(A), GenEyeBlock(A.cols))

  /* Takes in the cols of A, and the b vector to generate v, -v, zeros(A.cols), ones(A.cols) to set the constraints for each col. */
  def ExpandedVector(ACols: Int, b: DenseVector[Double]) = DenseVector.vertcat(b, -b, DenseVector.zeros[Double](ACols), DenseVector.ones[Double](ACols))

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

  /* Extracts one upper or lower bound from the system of A and b.
  @param Col The lambda you want to get an upperbound or lowerbound from (in Double format) */
  def ColBound(A: DenseMatrix[Double], b: DenseVector[Double], Col: Int, UpperOrLower: String): Double = {
    val c = NumberAmongZeros(1.0, A.cols, Col)
    val A_new = ExpandedMatrix(A)
    val b_new = ExpandedVector(A.cols, b)
    if (UpperOrLower == "Upper") {
      LowLevelSimplex(A_new, b_new, c)(Col)
    } else {
      LowLevelSimplex(A_new, b_new, -c)(Col)
    }
  }

  /*
  Compute Uppers Documentation:
  @param A DenseMatrix
  @param b DenseVector
  Takes in an A matrix and a b vector (representing an Ax <=b linear system) and outputs the upperbounds for each column of A [each lambda].)
   */
  def ComputeUppers(A: DenseMatrix[Double], b: DenseVector[Double]) = {
    var res = DenseVector.zeros[Double](A.cols)
    for (i <- 0 to A.cols - 1) {
      res(i) = ColBound(A, b, i, "Upper")
    }
    res
  }

  /*
Takes in an A matrix and a b vector (representing an Ax <=b linear system) and outputs the lowerbounds for each column of A [each lambda].)
 */

  def ComputeLowers(A: DenseMatrix[Double], b: DenseVector[Double]) = {
    var res = DenseVector.zeros[Double](A.cols)
    for (i <- 0 to A.cols - 1) {
      res(i) = ColBound(A, b, i, "Lower")
    }
    res
  }
}
