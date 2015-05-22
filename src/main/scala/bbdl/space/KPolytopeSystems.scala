package bbdl.space
import breeze.linalg.{DenseVector, DenseMatrix}
import breeze.util.JavaArrayOps

/*
This is a set of functions which help you set up the matrices for multiple A and b matrices, and comparisons between them.
In this implementation, we look specifically at how these spaces change over time, and thus the order of the System
Arrays is significant.
 */
object KSystemConstraints {
  /*
  A DenseMatrix[Double] is a matrix of any size
  j int is the index of the matrix among other matricies of the same size (0 indexed)
  K int the length of the array of systems
  @return A' A wider DenseMatrix[Double], with zeros where the other matrices would fit on both sides.
  When j=0 or j=k, A is padded on the right/left side, respectively
  Left side Zeros size = (m, nj)
  Right side Zeros size = (m, (K-1-j)*n
   */
  def PadWithZeroMatrices(A: DenseMatrix[Double], j: Int, K: Int): DenseMatrix[Double] = {
    if (j == 0) {
      //if it's the first matrix on the left
      DenseMatrix.horzcat(A, DenseMatrix.zeros[Double](A.rows, A.cols * (K - 1)))
    } else if (j == (K - 1)) {
      //if it's the last one on the right
      DenseMatrix.horzcat(DenseMatrix.zeros[Double](A.rows, A.cols * (K - 1)), A)
    } else {
      //if its a matrix in the middle somewhere, with zeros padded on both sides
      val FirstPartZeros = DenseMatrix.zeros[Double](A.rows, (K - 1 - j) * A.cols)
      val SecondPartZeros = DenseMatrix.zeros[Double](A.rows, A.cols * (K - j - 1))
      DenseMatrix.horzcat(Array(FirstPartZeros, A, SecondPartZeros): _*)
    }
  }

  def NegStackbVector(b: DenseVector[Double]): DenseVector[Double] = {
    DenseVector.vertcat(b, -b)
  }

  def NegStackAMatrices(A: DenseMatrix[Double]): DenseMatrix[Double] = {
    DenseMatrix.vertcat(A, -A)
  }

  /*
  @param kGeneratorSystems the set containing all of the consecutive A's b's and deltavecs
  @return equalityconstraints It adds all of them together in one big matrix
   */
  def ConcatConstraints_A(kGeneratorSystems: KGeneratorSystems): DenseMatrix[Double] = {
    val Ks = kGeneratorSystems.KSystemArray
    val PaddedAs = (0 to Ks.length - 1)
      .map(j => PadWithZeroMatrices(Ks(j).A, j, Ks.length)) //pad each of the matrices on both sides
      .map(Aj => NegStackAMatrices(Aj)) //do this to get an equality constraint instead of inequality
    DenseMatrix.vertcat(PaddedAs: _*)
  }
  def ConcatConstraints_b(kGeneratorSystems: KGeneratorSystems): DenseVector[Double] = {
    val Ks = kGeneratorSystems.KSystemArray
    val PaddedBs = (0 to Ks.length -1).map(j => DenseVector.vertcat(Ks(j).b, -Ks(j).b))
    DenseVector.vertcat(PaddedBs:_*)
  }

  /*
  @param Vector the constraints upon each of the muscles as you move from one system to the next
  @param j The index of the current location of the imposed vector
  @param Kn the total width of the matrix
  @return M a densevector that has padded zeros on both sides, according to how far it is from the left, as described
   by the j and Kn.
   */
  def PadWithZeroVector(Vector: DenseVector[Double], j: Int, Kn: Int): DenseVector[Double] = {
    if (j == 0) {
      DenseVector.vertcat(Vector, DenseVector.zeros[Double](Kn - Vector.length))
    } else if (j == Kn) {
      DenseVector.vertcat(DenseVector.zeros[Double](Kn - Vector.length), Vector)
    } else {
      val VecList = List(DenseVector.zeros[Double](j), Vector, DenseVector.zeros[Double](Kn - j - Vector.length))
      DenseVector.vertcat(VecList: _*)
    }
  }

  /*
  @param len the length of the desired vector
  @return V a vector of length len, with 1 as the first element, and -1 as the last element.
   */
  def PaddingPosNegOne(len: Int): DenseVector[Double] = {
    var temp = DenseVector.zeros[Double](len)
    temp.update(i = 0, 1)
    temp.update(i = len - 1, -1)
    temp
  }

  /*
  @brief Constructs a matrix of size
    @param n the number of variables in the system (muscles)
    @param K the number of timesteps
    */
  def deltaConstraintsA(n: Int, K: Int): DenseMatrix[Double] = {
    /*
    @brief  for 2 muscles, where j=0 and K=3, the row would be [1 0 -1 0 0 0] of type double.
    @param n the number of variables in the system (muscles)
    @param K the number of timesteps
    @param j the current column index of the leftmost element of the delta constraint (paddedposneg1 vector).
    @return a row of the constraint AMat
     */
    def row(n: Int, j: Int, K: Int): DenseVector[Double] = {
      PadWithZeroVector(PaddingPosNegOne(n + 1), j, K * n)
    }
    /*
    @brief stacks the constraints. to make it an inequality we double the constraints for positive and negative.
    two inequality constraints make an equality constraint.
    @param n the number of variables in the system (muscles)
    @param K the number of timesteps
     */
    def A(n: Int, K: Int): DenseMatrix[Double] = {
      val deltaA = (0 to K).toArray.map(j => row(n, j, K).toArray)
      val dm = JavaArrayOps.array2DToDm(deltaA)
      DenseMatrix.vertcat(dm, -dm)
    }
    A(n, K)
  }

  def deltaConstraintsb(kGeneratorSystems: KGeneratorSystems): DenseVector[Double] = {
    val Ks = kGeneratorSystems.KSystemArray
    val Deltas = (0 to Ks.length - 1).map(j => Ks(j).deltas)
    val DeltasArray = Deltas.toArray
    val DeltasA2 = DenseVector.vertcat(DeltasArray:_*)
    val ConcatDeltas = DenseVector.vertcat(DeltasA2, -DeltasA2)
    ConcatDeltas
  }
  def apply(kGeneratorSystems: KGeneratorSystems): (DenseMatrix[Double], DenseVector[Double]) = {
    val K = kGeneratorSystems.KSystemArray.length
    val n = kGeneratorSystems.KSystemArray(0).A.cols
    val AMatConstraints = ConcatConstraints_A(kGeneratorSystems)
    val AMatConstraintsB = ConcatConstraints_b(kGeneratorSystems)
    val LessThanOneA = DenseMatrix.ones[Double](K*n, K*n)
    val LessThanOneB = DenseVector.ones[Double](size=K*n)
    val LargerThanZeroA = DenseMatrix.zeros[Double](K*n, K*n)
    val LargerThanZeroB = DenseVector.zeros[Double](K*n)
    val DeltaConstraintsA = deltaConstraintsA(n,K)
    val DeltaConstraintsB = deltaConstraintsb(kGeneratorSystems)
    val ExpandedA = DenseMatrix.vertcat(Array(AMatConstraints,LessThanOneA,LargerThanZeroA,DeltaConstraintsA):_*)
    val ExpandedB = DenseVector.vertcat(Array(AMatConstraintsB,LessThanOneB, LargerThanZeroB, DeltaConstraintsB):_*)
    (ExpandedA,ExpandedB)
  }
}