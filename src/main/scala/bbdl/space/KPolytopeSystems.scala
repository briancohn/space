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
    // IMPORTANT - Ks.lengt is subtracted by 2 because we only want K-1 of the deltas.
    // We do not include the last delta from the last timepoint
    val Deltas = (0 to Ks.length - 2).map(j => Ks(j).deltas)
    val DeltasArray = Deltas.toArray
    val DeltasA2 = DenseVector.vertcat(DeltasArray:_*)
    val ConcatDeltas = DenseVector.vertcat(DeltasA2, DeltasA2)
    ConcatDeltas
  }
  def deltaConstraintsbForOneLessStep(kGeneratorSystems: KGeneratorSystems): DenseVector[Double] = {
    val Ks = kGeneratorSystems.KSystemArray
    // IMPORTANT - Ks.lengt is subtracted by 2 because we only want K-1 of the deltas.
    // We do not include the last delta from the last timepoint
    val Deltas = (0 to Ks.length - 3).map(j => Ks(j).deltas)
    val DeltasArray = Deltas.toArray
    val DeltasA2 = DenseVector.vertcat(DeltasArray:_*)
    val ConcatDeltas = DenseVector.vertcat(DeltasA2, DeltasA2)
    ConcatDeltas
  }
  /*
  returns a tuple of the stacked A and b for between 0 and 1
   */
  def ZeroOneBoundConstraints(n:Int): (DenseMatrix[Double], DenseVector[Double]) = {
    val NegativeEye = -DenseMatrix.eye[Double](n)
    val Zs = DenseVector.zeros[Double](n)
    val EyeMat = DenseMatrix.eye[Double](n)
    val OnesVec = DenseVector.ones[Double](n)
    val A = DenseMatrix.vertcat(NegativeEye,EyeMat)
    val B = DenseVector.vertcat(Zs, OnesVec)
    (A,B)
  }
  /*
  Concatenates the entire system including the bounds and delta constraints
   */
  def apply(kGeneratorSystems: KGeneratorSystems): (DenseMatrix[Double], DenseVector[Double]) = {
    val K = kGeneratorSystems.KSystemArray.length
    val n = kGeneratorSystems.KSystemArray(0).A.cols
    val AMatConstraints = ConcatConstraints_A(kGeneratorSystems)
    val AMatConstraintsB = ConcatConstraints_b(kGeneratorSystems)
    //bound constraints
    val NegativeEye = -DenseMatrix.eye[Double](K*n)
    val Zs = DenseVector.zeros[Double](K*n)
    val EyeMat = DenseMatrix.eye[Double](K*n)
    val OnesVec = DenseVector.ones[Double](K*n)
//TODO refactor use ZeroOneBoundConstraints
    val DeltaConstraintsA = deltaConstraintsA(n,K)
    val DeltaConstraintsB = deltaConstraintsb(kGeneratorSystems)
    val ExpandedA = DenseMatrix.vertcat(Array(AMatConstraints,NegativeEye, EyeMat, DeltaConstraintsA):_*)
    val ExpandedB = DenseVector.vertcat(Array(AMatConstraintsB, Zs, OnesVec, DeltaConstraintsB):_*)
    (ExpandedA,ExpandedB)
  }
}

object KSystemConstraintsAbsDiffDelta {
  def AuxVars(K: Int, n: Int): DenseMatrix[Double] = {
    val EyeMat = DenseMatrix.eye[Double](n*(K-1))
    DenseMatrix.vertcat(-EyeMat,-EyeMat)
  }

  def apply(kGeneratorSystems: KGeneratorSystems): (DenseMatrix[Double], DenseVector[Double]) = {
    val K = kGeneratorSystems.KSystemArray.length
    val n = kGeneratorSystems.KSystemArray(0).A.cols
  // construct the system constraints
    val A1 = KSystemConstraints.ConcatConstraints_A(kGeneratorSystems)
    val B1 = KSystemConstraints.ConcatConstraints_b(kGeneratorSystems)
    // construct bound constraints
    val BoundConstraints = KSystemConstraints.ZeroOneBoundConstraints(K*n)
    val A2 = BoundConstraints._1
    val B2 = BoundConstraints._2
    //construct delta constraints
    val A3 = KSystemConstraints.deltaConstraintsA(n,K)
    val B3 = KSystemConstraints.deltaConstraintsb(kGeneratorSystems)
    //concat KSystemConstraints
    val KSystemCols = DenseMatrix.vertcat(Array(A1,A2,A3,A3):_*)

    val A4 = AuxVars(K,n)

    val RowsA1A2A3 = A1.rows+A2.rows+A3.rows
    val AuxZeros = DenseMatrix.zeros[Double](RowsA1A2A3, (K-1)*n)
    val AuxCols = DenseMatrix.vertcat(AuxZeros,A4)
    val AWithAux = DenseMatrix.horzcat(KSystemCols, AuxCols)
    val BWithAux = DenseVector.vertcat(Array(B1,B2,B3, DenseVector.zeros[Double](A3.rows)):_*)
    (AWithAux,BWithAux)
  }
}

object KSystemCheckPoint {
  /*
  j is out of K steps
   */
  def apply(kGeneratorSystems: KGeneratorSystems, PointInQuestion: DenseVector[Double], j: Int): Boolean = {
    val MinusLastOne = DropLastSystem(kGeneratorSystems)
    val K = kGeneratorSystems.KSystemArray.length
    val n = kGeneratorSystems.KSystemArray(0).A.cols
    val AConstraints = KSystemConstraints.ConcatConstraints_A(MinusLastOne)
    val BConstraints = KSystemConstraints.ConcatConstraints_b(MinusLastOne)
    val Bounds = KSystemConstraints.ZeroOneBoundConstraints((K-1)*n)
    val ADelta = KSystemConstraints.deltaConstraintsA(n,K-1)
    val BDelta = KSystemConstraints.deltaConstraintsbForOneLessStep(kGeneratorSystems)

    // this final constraint is for the point with respect to the last j (which is j=k)
    val ADeltasLast = LastDeltaConstraint(n,K)
    val UltDeltaPoint = UltimatePointDeltaB(kGeneratorSystems,PointInQuestion)
    val A = DenseMatrix.vertcat(Array(AConstraints, Bounds._1, ADelta,ADeltasLast):_*)
    val Constraints = Array(
    BConstraints,
      Bounds._2,
      BDelta,
    UltDeltaPoint)
    val B = DenseVector.vertcat(Constraints:_*)
    (A,B)
    true
  }
  def LastDeltaConstraint(n:Int ,K:Int): DenseMatrix[Double] = {
    val LeftPadding = DenseMatrix.zeros[Double](n*(K-2),n*(K-2))
    val Eye = DenseMatrix.eye[Double](n)
    val RightConstraints = DenseMatrix.vertcat(Eye,-Eye)
    //Combine them together
    DenseMatrix.horzcat(LeftPadding,RightConstraints)
  }
  /*
  only returns the first N entries
   */
  def DropLastSystem(kGeneratorSystems: KGeneratorSystems): KGeneratorSystems = {
    val Length = kGeneratorSystems.KSystemArray.length
    KGeneratorSystems(kGeneratorSystems.KSystemArray.take(Length-1))
  }
  def UltimatePointDeltaB(kGeneratorSystems: KGeneratorSystems, Point: DenseVector[Double]): DenseVector[Double] = {
    val PenultimateK = kGeneratorSystems.KSystemArray(kGeneratorSystems.KSystemArray.length-2)
    val Positives = PenultimateK.deltas + Point
    val Negatives = PenultimateK.deltas - Point
    DenseVector.vertcat(Positives,Negatives)
  }

}