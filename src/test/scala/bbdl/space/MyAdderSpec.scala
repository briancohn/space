package bbdl.space
import java.io.File
import breeze.linalg._
import breeze.numerics._
import breeze.math._
import org.scalacheck.Prop.False
import org.scalatest._
import Matchers._
import scala.util.Random

class MaximumOutputSpec extends FlatSpec with Matchers {
  behavior of "Maximum Output Spec"
    val A = DenseMatrix(
      (10.0/3.0, -53.0/15.0, 2.0)
    )
    val b = DenseVector(1.0)
    val MaxInfo = MaximumOutput(A,b)
    val FOutputVector = MaxInfo._1
    val ActivationVec = MaxInfo._2
  it should "Get correct maximum force vector" in {
//    16.0/3.0 is 5.333333333333334
    assert(FOutputVector == DenseVector(5.333333333333334))
  }
  it should "Get correct activation vector" in {
    assert(ActivationVec == DenseVector(1.0,0.0,1.0))
  }
  //set up the A matrix for the index finger in 7 d
  val JRIndex = DenseMatrix(
    (-0.08941, -0.0447, 0.2087, -0.2138, -0.009249, 0.1421, 0.03669),
    (-0.04689, -0.1496, 0.0, 0.0248, 0.052, 0.0248, 0.052),
    (0.06472, 0.001953, 0.0568, 0.2067, -0.1518, 0.2919, -0.1518),
    (0.003081, -0.002352, 0.0001578, -0.000685, -0.0001649, -0.0004483, -0.0001649)
  )
  val FmIndex = DenseVector(123,219,124.8,129.6,23.52,21.6,91.74)
  val AIndex = JRIndex*diag(FmIndex)
  val bIndex_y = DenseVector(0.0,1.0,0.0,0.0)//this is the direction we will maximize into.
  val IndexMaxInfo = MaximumOutput(AIndex, bIndex_y)
  val FOutputVectorIndex = IndexMaxInfo._1
  val IndexAVec = IndexMaxInfo._2
  it should "Get correct maximum force vector where y is maximized on the FVC index finger. in 7d" in {
    val diff = AbsDiff.Vectors(FOutputVectorIndex, DenseVector(0.0, 6.757881713139706, 0.0, 0.0))
    assert(diff < 1E-13)//pasted output - test generated for sustainability tracking
  }
  it should "Get correct activation vector where y is maximized on the FVC index finger in 7D." in {
    val diff = AbsDiff.Vectors(IndexAVec, DenseVector(0.14390467823812347, 0.0, 0.1724420617559178, 0.3293781199403045, 1.0, 1.0, 1.0))
    assert(diff < 1E-13)
  }
  it should "Generate force when using the a solution and the Fm matrix" in {
    val ExpF = AIndex*IndexAVec
    val AbsError = AbsDiff.Matricies(ExpF.toDenseMatrix, FOutputVectorIndex.toDenseMatrix)
    assert(AbsError < 1E-12)
  }
  it should "Get correct maximum force vector where z is maximized on the FVC index finger in 7d" in {
    val bIndex_z = DenseVector(0.0,0.0,1.0,0.0)
    val IndexMaxInfo_z = MaximumOutput(AIndex, bIndex_z)
    val diff = AbsDiff.Vectors(IndexMaxInfo_z._1, DenseVector(0.0, 0.0, 40.28419614785415, 0.0))
    assert(diff < 1E-13)
  }
  it should "Get correct activation vector where z is maximized on the FVC index finger in 7d" in {
    val bIndex_z = DenseVector(0.0,0.0,1.0,0.0)
    val IndexMaxInfo_z = MaximumOutput(AIndex, bIndex_z)
    val diff = AbsDiff.Vectors(IndexMaxInfo_z._2, DenseVector(0.27027228376106427, 0.05927684709966398, 1.0, 0.922552974363257, 0.0, 1.0, 0.0))
    assert(diff <= 1E-14)
  }
  it should "Get correct maximum force vector where y and z is maximized on the FVC index finger in 7d" in {
    val bIndex_yz = DenseVector(0.0,1.0,1.0,0.0)
    val IndexMaxInfo_yz = MaximumOutput(AIndex, bIndex_yz)
    val diff = AbsDiff.Vectors(IndexMaxInfo_yz._1, DenseVector(0.0, 7.195388104572386, 7.195388104572386, 0.0))
    assert(diff < 1E-13)
  }
  it should "Get correct activation vector where y and z is maximized on the FVC index finger in 7d" in {
    val bIndex_yz = DenseVector(0.0,1.0,1.0,0.0)
    val IndexMaxInfo_yz = MaximumOutput(AIndex, bIndex_yz)
    val diff = AbsDiff.Vectors(IndexMaxInfo_yz._2, DenseVector(0.17866959252521905, 0.0, 0.3982986123112096, 0.5278834439011518, 1.0, 1.0, 0.9999999999999999))
    assert(diff < 1E-14)
  }
}

class GetRandomDirectionSpec extends FlatSpec with Matchers {
	behavior of "GetRandomDirection"
  val Seed = 10
  val RandomObject = new scala.util.Random(Seed)
  it should "Get a random direction for all positive inputs" in {
    val B = DenseMatrix((1.0,0.0), (0.0,1.0), (0.0, 0.0))

    val result = GetRandomDirection(B, RandomObject)
    assert(result === DenseVector(1.6882124514042414, -0.8414172955978727, 0.0))
	}
	it should "get a random direction for some negative inputs" in {
	  val B = DenseMatrix((-1.0,0.0), (0.0,1.0), (0.0, 0.0))
	  val RandomDirection = GetRandomDirection(B, RandomObject)
	  val expected = DenseVector(0.32113532510702947, -0.23661697576709095, 0.0)
    assert(RandomDirection === expected)
	}
  it should "take in an identity matrix, and output a vector whose values are distributed gauss-normally" in{
    val n = 10
    val IdentityMatrix = DenseMatrix.eye[Double](n)
    val RandomDirection = GetRandomDirection(IdentityMatrix, RandomObject)
    assert(RandomDirection === DenseVector(-0.5241639211111205, -0.34602233612988537, 1.1638138842834067, -1.662967347612762, -0.5866125475069541, 1.3084986313549418, -0.7113249308260786, -0.9384431706369464, 0.005116309664665023, 0.3555017051770467))
  }
  it should "take in an identity matrix with an extra row of zeros at the bottom, and output a vector whose values are distributed gauss-normally" in{
    val n = 10
    val IdentityMatrix = DenseMatrix.eye[Double](n)
    val TallerMatrix = DenseMatrix.vertcat(IdentityMatrix, DenseMatrix.zeros[Double](1,n))
    val RandomDirection = GetRandomDirection(TallerMatrix, RandomObject)
    assert(AbsDiff.Matricies(RandomDirection.toDenseMatrix, DenseVector(0.6685391145389219, 0.6327216675776454, -0.9424452549137063, -0.009898782433063143, 1.8021608700963798, 1.8765599210333714, 0.5111602993013169, 0.8418645608663314, -0.8955260348019763, -0.4938850345409942, 0.0).toDenseMatrix) < 1E-14)
  }

  val RandomObject7 = new scala.util.Random(7)
  it should "produce a gauss-distribution with mean 0 and standard deviation 1 on an identity matrix" in {
    val x = GetRandomDirection(breeze.linalg.DenseMatrix.eye[Double](10), RandomObject7)
    assert(x === DenseVector(0.8452060657049847, 0.9128761787534405, -0.2870786364749953, 0.7518594314874758, 1.335473668231534, -0.9499789372646104, 0.599049892177836, 1.204570743449295, 2.4820093995603614, -0.7539501059617072))
  }
  import bbdl.space.GetRandomDirection
  import breeze.linalg.DenseMatrix
  val n = 1000
  val Mat = DenseMatrix.eye[Double](n)
  val Direction = GetRandomDirection(Mat, RandomObject)
  val Mu = breeze.stats.mean(Direction)
  val Sigma = breeze.stats.stddev(Direction)

  it should "produce a gaussian distribution with mean 0 with a very large matrix" in {
    assert(abs(Mu) < 0.10)
  }
  it should "produce a gaussian distribution with SD 1 with a very large matrix" in {
    assert(abs(Sigma - 1.0) < 0.05)
  }
  "GetNewPoint" should "Produce a new random point when given a point and random direction" in {
    val RandomObject = new Random(10)
    val p = DenseVector(0.5,0.5,0.5)
    val q = DenseVector(-1.0,-2.0,1.0)
    val g = GetNewPoint(p,q,RandomObject)
    val ExpectedPoint = DenseVector(0.3847848516282864, 0.2695697032565728, 0.6152151483717136)
    assert(g === ExpectedPoint)
  }
}

class BasisSpec extends FlatSpec with Matchers {
	behavior of "Basis"
	it should "take in a matrix of size (2,5), and output a (5,2) basis" in {
    import bbdl.space.Basis
    import breeze.linalg._
    val A = DenseMatrix(
      (1.0, 1.0, 0.0, 0.0, 1.0),
      (0.0, 1.0, 1.0, 1.0, 1.0),
      (1.0, 0.0, 2.0, 1.0, 1.0)
    )
    val basis = Basis(A)
    val ExpectedBasis = DenseMatrix((1,0),(0,1), (-1,1),(2, -1),(-1, -1))
    assert(basis == ExpectedBasis)
	}
  it should "take in a matrix of size(2,4) and output a (4,2) basis" in {
    val A = DenseMatrix(
      (1.0, 1.0, 1.0, 1.0),
      (1.0, 1.0, 2.0, 1.0)
    )
    val basis=Basis(A)
    val ExpectedBasis = DenseMatrix(
      (1.0, 0.0),
      (0.0,1.0),
      (0.0,0.0),
      (-1.0,-1.0)
    )
    assert(basis === ExpectedBasis)
  }
  it should "take in a matrix of size(1,3) and output a (3,2) basis" in
    { val A = DenseMatrix(
      (10.0/3.0, -53.0/15.0, 2.0)
    )
      val basis=Basis(A)
      val ExpectedBasis = DenseMatrix(
        (1.0, 0.0),
        (0.0,1.0),
        (-5.0/3.0,53.0/30.0)
      )
      assert(basis === ExpectedBasis)
    }

}

class OrthoSpec extends FlatSpec with Matchers {
	behavior of "orthonormalize"
	it should "take in a matrix of size (3,3); the basis" in {
    val BasisOrthonormal = Ortho( DenseMatrix((1.0,1.0),(0.0,1.0)) )
	  val ExpectedBasisOrthonormal = DenseMatrix((1.0,0.0), (0.0,1.0))
	  assert(BasisOrthonormal === ExpectedBasisOrthonormal)

	}
  it should "take in a matrix of size (3,2) and output a (3,2) orthogonal basis" in {
    val A = DenseMatrix(
    (1.0, 0.0),
    (0.0,1.0),
    (-5.0/3.0,53.0/30.0)
    )
    val Orthobasis = Ortho(A)
    import breeze.linalg.{DenseVector, norm, DenseMatrix}
    val a = norm(DenseVector(53.0/68.0, 1.0, 159.0/340.0))
    val ExpectedOrthoBasis = DenseMatrix(
      (3.0/sqrt(34.0), 53.0/(68.0*a)),
      (0.0,1.0/a),
      (-5.0/sqrt(34.0),159.0/(340.0*a))
    )
    val AbsError = AbsDiff.Matricies(Orthobasis, ExpectedOrthoBasis)
    assert(AbsError < 1E-14)
  }
}

class GetNewPointSpec extends FlatSpec with Matchers {
  val Seed = 10
  "GetEndpoints" should "Get endpoints for a point and a positive direction" in {
    val p = DenseVector(0.5,0.5,0.5)
    val q = DenseVector(-1.0,-2.0,1.0)
    val Endpoints = GetNewPoint.GetEndpoints(p,q)
    val ExpectedEndpoints = (DenseVector(0.75,1.0,0.25), DenseVector(0.25,0.0,0.75))
    assert(Endpoints === ExpectedEndpoints)
  }

  "GetEndpoints" should "Get endpoints for a point and a negative direction" in {
    val p = DenseVector(0.0,0.5,0.5)
    val q = DenseVector(2.0,1.0,2.0)
    val Endpoints = GetNewPoint.GetEndpoints(p,q)
    val ExpectedEndpoints = (DenseVector(0.0,0.5,0.5), DenseVector(0.5,0.75,1.0))
    assert(Endpoints === ExpectedEndpoints)
  }

  behavior of "UpperboundVal"
  it should "take in a 3d point and positive 3d direction, and output a 3d upperbound vector" in {
    val p = DenseVector(0.0,0.5,0.5)
    val q = DenseVector(2.0,1.0,2.0)
    val UpperBounds = GetNewPoint.GetUpperBoundVector(p,q)
    val ExpectedUpperBounds = DenseVector(0.5,0.5,0.25)
    assert(UpperBounds === ExpectedUpperBounds)
  }
  behavior of "LowerboundVal"
  it should "take in a 3d point and positive 3d direction, and output a 3d LowerBound vector" in {
    val p = DenseVector(0.0,0.5,0.5)
    val q = DenseVector(2.0,1.0,2.0)
    val UpperBounds = GetNewPoint.GetLowerBoundVector(p,q)
    val ExpectedUpperBounds = DenseVector(0.0,-0.5,-0.25)
    assert(UpperBounds === ExpectedUpperBounds)
  }
  behavior of "UpperboundVal"
  it should "take in a 3d point and negative 3d direction, and output a 3d upperbound vector" in {
    val p = DenseVector(0.5,0.5,0.5)
    val q = DenseVector(-1.0,-2.0,1.0)
    val UpperBounds = GetNewPoint.GetUpperBoundVector(p,q)
    val ExpectedUpperBounds = DenseVector(0.5,0.25,0.5)
    assert(UpperBounds === ExpectedUpperBounds)
  }
  behavior of "LowerboundVal"
  it should "take in a 3d point and negative 3d direction, and output a 3d LowerBound vector" in {
    val p = DenseVector(0.5,0.5,0.5)
    val q = DenseVector(-1.0,-2.0,1.0)
    val UpperBounds = GetNewPoint.GetLowerBoundVector(p,q)
    val ExpectedUpperBounds = DenseVector(-0.5,-0.25,-0.5)
    assert(UpperBounds === ExpectedUpperBounds)
    assert(UpperBounds === ExpectedUpperBounds)
    assert(UpperBounds === ExpectedUpperBounds)
    assert(UpperBounds === ExpectedUpperBounds)
  }
  //Set up a test for upper and lower bounds

  val Lowers = DenseVector(-1.0,-6.0,-4.0,-5.0,-3.0,-5.0)
  val Uppers = DenseVector(8.0,7.0,9.0,8.0,9.0,10.0)
  val PositiveBounds = GetNewPoint.GetBoundLimits(Uppers, Lowers)
  "GetBoundLimits" should "Return the max value within the lower bounds vector " in {
    assert(PositiveBounds._2 === -1.0)
  }
  it should "Return the minimum value within the upper bounds vector" in {
    assert(PositiveBounds._1 === 7.0)
  }

  
  "FindEndpoints" should "use the p, negative direction q, and bound information to assemble two endpoints" in {
    val p = DenseVector(0.5,0.5,0.5)
    val q = DenseVector(-1.0,-2.0,1.0)
    val LowerBoundInner = -0.25
    val UpperBoundInner = 0.25
    val Endpoints = GetNewPoint.FindEndpoints(p,q, UpperBoundInner,LowerBoundInner)
    val ExpectedEndpoints = (DenseVector(0.75,1.0,0.25), DenseVector(0.25,0.0,0.75))
    assert(Endpoints === ExpectedEndpoints)
  }
  it should "use the p, positive direction q, and bound information to assemble two endpoints" in {
    val p = DenseVector(0.0,0.5,0.5)
    val q = DenseVector(2.0,1.0,2.0)
    val LowerBoundInner = 0.0
    val UpperBoundInner = 0.25
    val Endpoints = GetNewPoint.FindEndpoints(p,q, UpperBoundInner,LowerBoundInner)
    val ExpectedEndpoints = (DenseVector(0.0,0.5,0.5), DenseVector(0.5,0.75,1.0))
    assert(Endpoints === ExpectedEndpoints)
  }
}

class LowLevelSimplexSpec() extends FlatSpec with Matchers {
  behavior of "LowLevelSimplex"
  it should "take in a (3,2) matrix and vector of len 3, and 2len c, and return vector of len 2. " in {
    val A = DenseMatrix(
      (1.0,2.0),
      (1.0,1.0),
      (-1.0,0.0)
      )
    val b = DenseVector(4.0,3.0,0.0)
    val c = DenseVector(0.0,1.0)
    val x_expected = DenseVector(0.0,2.0)
    assert(LowLevelSimplex(A,b,c) == x_expected)
  }
}

class GenStartingPointSpec() extends FlatSpec with Matchers {
  behavior of "ExpandAMatrix"
  it should "take in a simple 2,3 matrix and output the expanded A" in {
    val A = DenseMatrix((1.0,2.0,3.0),(4.0,5.0,6.0))
    // val G = DenseMatrix((1.0,2.0), (3.0,4.0))

    val AExpected = DenseMatrix(
      (1.0,2.0,3.0,0.0,0.0,0.0),
      (4.0,5.0,6.0,0.0,0.0,0.0),
      (-1.0,-2.0,-3.0,0.0,0.0,0.0),
      (-4.0,-5.0,-6.0,0.0,0.0,0.0),
      (-1.0,0.0,0.0,1.0,0.0,0.0),
      (0.0,-1.0,0.0,0.0,1.0,0.0),
      (0.0,0.0,-1.0,0.0,0.0,1.0),
      (1.0,0.0,0.0,1.0,0.0,0.0),
      (0.0,1.0,0.0,0.0,1.0,0.0),
      (0.0,0.0,1.0,0.0,0.0,1.0),
      (0.0,   0.0,   0.0,   -1.0,  -0.0,  -0.0),
      (0.0,   0.0,   0.0,   -0.0,  -1.0,  -0.0),
      (0.0,   0.0,   0.0,   -0.0,  -0.0,  -1.0) 
      )

    val myA = GenStartingPoint.ExpandAMatrix(A)
    assert(myA == AExpected)
  }
  behavior of "ExpandbVector"
  it should "take in a len 2 vector and output a len 10 vector expanded" in {
    val A = DenseMatrix((1.0,2.0,3.0),(4.0,5.0,6.0))
    val ColNum = A.cols
    val b = DenseVector(7.0,8.0)
    val ExpandedbVector = GenStartingPoint.ExpandbVector(b, ColNum)
    val Expectedb= DenseVector(7.0,8.0,-7.0,-8.0,0.0,0.0,0.0,1.0,1.0,1.0,0.0,0.0,0.0)
    assert(ExpandedbVector == Expectedb)
  }
  behavior of "GencVector"
  it should "take in a 2,3 matrix and output the zeros and ones; a len 6 vector" in {
    val A = DenseMatrix((1.0,2.0,3.0),(4.0,5.0,6.0))
    val ColNum = A.cols
    val TestC = GenStartingPoint.GencVector(A)
    val Expectedc= DenseVector(0.0,0.0,0.0,1.0,1.0,1.0)
    assert(TestC == Expectedc)
  }
}

class RandomPointBetweenSpec() extends FlatSpec with Matchers {
  behavior of "RandomPointBetween"
  val RandomObject7 = new scala.util.Random(7)
  it should "Take in two points and return a point on the line between them. An example with a zero in both" in {
    val E1 = DenseVector(0.75,0.5,0)
    val E2 = DenseVector(0.6,0,0.2)
    val MyPoint = RandomPointBetween(E1,E2,RandomObject7)
    val ExpectedPoint = DenseVector(0.6403951436909937, 0.13465047896997895, 0.14613980841200844)
    assert(MyPoint === ExpectedPoint)
  }
  "Randpt" should "Take in two points and return a point on the line between them. Example with no zero" in {
    val E1 = DenseVector(0.5,0.2,0.7)
    val E2 = DenseVector(0.1,0.8,0.9)
    val MyPoint = RandomPointBetween(E1,E2,RandomObject7)
    val ExpectedPoint = DenseVector(0.20033215874654675, 0.6495017618801799, 0.8498339206267267)
    assert(MyPoint === ExpectedPoint)
  }
  "Randpt" should "Take in two points and return a point on the line between them. Example with ones" in {
    val E1 = DenseVector(1.0,0.2,0.7)
    val E2 = DenseVector(0.1,0.8,1.0)
    val MyPoint = RandomPointBetween(E1,E2,RandomObject7)
    val ExpectedPoint = DenseVector(0.6865212672718688, 0.4089858218187542, 0.804492910909377)
    assert(MyPoint === ExpectedPoint)
  }
}

class UpdateMeanSpec() extends FlatSpec with Matchers{
  "updateMean" should "take in a prior mean (and an n) and update the running mean with a new value" in {
    val n = 5.0
    val PriorMean = 100.0
    val NewValue = 200.0
    val result = UpdateMean(NewValue, PriorMean, n)
    assert(result == 120.0)
  }
}

class BoundsSpec() extends FlatSpec with Matchers{
  "Bounds.Expanded Matrix" should "Properly form an expanded matrix from a 2,3 example" in {
    val A = DenseMatrix(
      (2.0,1.0,2.0),
      (1.0,1.0,3.0)
    )
    val res = Bounds.ExpandedMatrix(A)
    val expected = DenseMatrix(
      (2.0, 1.0, 2.0),
      (1.0, 1.0, 3.0),
      (-2.0, -1.0, -2.0),
      (-1.0, -1.0, -3.0),
      (-1.0, 0.0, 0.0),
      (0.0, -1.0, 0.0),
      (0.0, 0.0, -1.0),
      (1.0, 0.0, 0.0),
      (0.0, 1.0, 0.0),
      (0.0, 0.0, 1.0)
    )

    assert(res == expected)
  }
  "Bounds.ExpandedVector" should "Properly form an expanded vector from a 4 element example" in {
    val v = DenseVector(1.0,4.0,3.0)
    val ACols = 3
    val res = Bounds.ExpandedVector(ACols, v)
    val expected = DenseVector(1.0,4.0,3.0,-1.0,-4.0,-3.0, 0.0,0.0,0.0,1.0,1.0,1.0)
    assert(res == expected)
  }
  "Bounds.NumberAmongOnes" should "add a 1.0 between a bunch of zeros for a 5 element vector" in {
    assert(Bounds.NumberAmongZeros(1.0,5,2) == DenseVector(0.0,0.0,1.0,0.0,0.0))
  }
  val A = DenseMatrix(
    (10.0/3.0, -53.0/15.0, 2.0)
  )
  val v = DenseVector(1.0)
  "Bounds.ColBound" should "Be 1 for upperbound for a 1D output, 3D input, simple example" in {
    assert(Bounds.ColBound(A,v,0, "Upper") == 1.0)
    assert(Bounds.ColBound(A,v,1, "Upper") == 1.0)
    assert(Bounds.ColBound(A,v,2, "Upper") == 1.0)
  }
  "Bounds.ColBound" should "Be 0 for lowerbound for a 1D output, 3d,input, simple example" in {
    assert(Bounds.ColBound(A,v,0,"Lower") == 0.0)
    assert(Bounds.ColBound(A,v,1, "Lower") == 0.0)
    assert(Bounds.ColBound(A,v,2, "Lower") == 0.0)
  }
  "Bounds.ColBound" should "Be one value from"
  "Bounds.ComputeUppers" should "Computer the Upperbounds of 111 for the simple 3dinput,1D output example" in {
    assert(Bounds.ComputeUppers(A,v)== DenseVector(1.0,1.0,1.0))
  }
  "Bounds.ComputeLowers" should "Compute the Lowerbounds of 000 for the simple 3dinput, 1D output example" in {
    assert(Bounds.ComputeLowers(A,v)== DenseVector(0.0,0.0,0.0))
  }
}

class VectorScaleSpec() extends FlatSpec with Matchers{
  "VectorScale.apply" should "scale by fifty percent for a simple xy vector" in {
    val ScaledVector = VectorScale(DenseVector(1.0,0.0),0.5)
    assert(ScaledVector == DenseVector(0.5,0.0))
  }
}

class PointStreamSpec() extends FlatSpec with Matchers {
  behavior of "PointStream"
  "PointStream" should "generate n points when using PointStream.generate(n)" in {
    import bbdl.space._
    import breeze.linalg._
    import breeze.numerics._
    import breeze.stats._
    val Seed = 10
    val RandomObject = new scala.util.Random(Seed)
    val JR = DenseMatrix(
      (-0.08941, -0.0447, 0.2087, -0.2138, -0.009249, 0.1421, 0.03669),
      (-0.04689, -0.1496, 0.0, 0.0248, 0.052, 0.0248, 0.052),
      (0.06472, 0.001953, 0.0568, 0.2067, -0.1518, 0.2919, -0.1518),
      (0.003081, -0.002352, 0.0001578, -0.000685, -0.0001649, -0.0004483, -0.0001649)
    )
    val Fm = DenseVector(123,219,124.8,129.6,23.52,21.6,91.74)
    val A = JR*diag(Fm)
    val v = DenseVector(1.0,1.0,1.0,0.0)
    val OrthonormalBasis = Ortho(Basis(A)) //Orthogonalize the basis
    val CurrentPoint = GenStartingPoint(A, v)
    val db = PointStream.generate(10,OrthonormalBasis,CurrentPoint,RandomObject)
    assert(db.rows == 10)
  }
  "PointStream" should "generate n points in a certain direction, where alpha is 0.5 (n=10)" in {
    import bbdl.space._
    import breeze.linalg._
    import breeze.numerics._
    import breeze.stats._
    val Seed = 10
    val RandomObject = new scala.util.Random(Seed)
    val JR = DenseMatrix(
      (-0.08941, -0.0447, 0.2087, -0.2138, -0.009249, 0.1421, 0.03669),
      (-0.04689, -0.1496, 0.0, 0.0248, 0.052, 0.0248, 0.052),
      (0.06472, 0.001953, 0.0568, 0.2067, -0.1518, 0.2919, -0.1518),
      (0.003081, -0.002352, 0.0001578, -0.000685, -0.0001649, -0.0004483, -0.0001649)
    )
    val Fm = DenseVector(123,219,124.8,129.6,23.52,21.6,91.74)
    val A = JR*diag(Fm)
    val v = DenseVector(1.0,1.0,1.0,0.0) //xy direction
    val vPrime = VectorScale(v,0.999)
    val OrthonormalBasis = Ortho(Basis(A)) //Orthogonalize the basis
    val CurrentPoint = GenStartingPoint(A, vPrime)
    val db = PointStream.generate(10,OrthonormalBasis,CurrentPoint,RandomObject)
    assert(db.cols==7)
    assert(db.rows==10)
    val FileName = Output.TimestampCSVName("output/XYZoneoneone").toString()
    val MyFile = new java.io.File(FileName)
    csvwrite(MyFile, db)

  }
  "PointStream" should "generate n points in a direction progression where alpha is increasing (n=2)" in {
    import bbdl.space._
    import breeze.linalg._
    import breeze.numerics._
    import breeze.stats._
    val Seed = 10
    val RandomObject = new scala.util.Random(Seed)
    val JR = DenseMatrix(
      (-0.08941, -0.0447, 0.2087, -0.2138, -0.009249, 0.1421, 0.03669),
      (-0.04689, -0.1496, 0.0, 0.0248, 0.052, 0.0248, 0.052),
      (0.06472, 0.001953, 0.0568, 0.2067, -0.1518, 0.2919, -0.1518),
      (0.003081, -0.002352, 0.0001578, -0.000685, -0.0001649, -0.0004483, -0.0001649)
    )
    val Fm = DenseVector(123,219,124.8,129.6,23.52,21.6,91.74)
    val A = JR*diag(Fm)
    val v = DenseVector(1.0,1.0,1.0,0.0) //xy direction
    val OrthonormalBasis = Ortho(Basis(A)) //Orthogonalize the basis
    val db = PointStream.alphaGenerate(2, Tuple2(0.0, 0.9),10, v, A, OrthonormalBasis, RandomObject)
    db.cols should equal (12) // lambdas+v_i's+alpha
    db.rows should equal (20)
  }
  "PointStream" should "generate 10 points( per alpha) in a direction progression where alpha is increasing in the xyz direction." in {
    val Seed = 10
    val RandomObject = new scala.util.Random(Seed)
    val JR = DenseMatrix(
      (-0.08941, -0.0447, 0.2087, -0.2138, -0.009249, 0.1421, 0.03669),
      (-0.04689, -0.1496, 0.0, 0.0248, 0.052, 0.0248, 0.052),
      (0.06472, 0.001953, 0.0568, 0.2067, -0.1518, 0.2919, -0.1518),
      (0.003081, -0.002352, 0.0001578, -0.000685, -0.0001649, -0.0004483, -0.0001649)
    )
    val Fm = DenseVector(123,219,124.8,129.6,23.52,21.6,91.74)
    val A = JR*diag(Fm)
    val v = DenseVector(1.0,1.0,1.0,0.0) //xy direction
    val OrthonormalBasis = Ortho(Basis(A)) //Orthogonalize the basis
    val AlphaLenOut = 10
    val PointsPerAlpha = 10

    val db = PointStream.alphaGenerate(PointsPerAlpha, Tuple2(0.0, 0.9),AlphaLenOut, v, A, OrthonormalBasis, RandomObject)
    db.cols should equal (12)
    db.rows should equal (100)
    val FileName = Output.TimestampCSVName("output/XYZ_alphaProgression").toString()
    val MyFile = new java.io.File(FileName)
    csvwrite(MyFile, db)
  }
  "PointStream" should "generate 100 points( per alpha) in a direction progression where alpha is increasing in the x direction." in {
    import bbdl.space._
    import breeze.linalg._
    import breeze.numerics._
    import breeze.stats._
    val Seed = 10
    val RandomObject = new scala.util.Random(Seed)
    val JR = DenseMatrix(
      (-0.08941, -0.0447, 0.2087, -0.2138, -0.009249, 0.1421, 0.03669),
      (-0.04689, -0.1496, 0.0, 0.0248, 0.052, 0.0248, 0.052),
      (0.06472, 0.001953, 0.0568, 0.2067, -0.1518, 0.2919, -0.1518),
      (0.003081, -0.002352, 0.0001578, -0.000685, -0.0001649, -0.0004483, -0.0001649)
    )
    val Fm = DenseVector(123,219,124.8,129.6,23.52,21.6,91.74)
    val A = JR*diag(Fm)
    val v = DenseVector(1.0,0.0,0.0,0.0) //xy direction
    val OrthonormalBasis = Ortho(Basis(A)) //Orthogonalize the basis
    val AlphaLenOut = 10
    val PointsPerAlpha = 100
    val db = PointStream.alphaGenerate(PointsPerAlpha, Tuple2(0.0, 0.9),AlphaLenOut, v, A, OrthonormalBasis, RandomObject)
    db.cols should equal (12)
    db.rows should equal (1000)
    val FileName = Output.TimestampCSVName("output/X_alphaProgression").toString()
    val MyFile = new java.io.File(FileName)
    csvwrite(MyFile, db)
  }
  "PointStream" should "generate 100 points( per alpha) in a direction progression where alpha is increasing in the y direction." in {
    import bbdl.space._
    import breeze.linalg._
    import breeze.numerics._
    import breeze.stats._
    val Seed = 10
    val RandomObject = new scala.util.Random(Seed)
    val JR = DenseMatrix(
      (-0.08941, -0.0447, 0.2087, -0.2138, -0.009249, 0.1421, 0.03669),
      (-0.04689, -0.1496, 0.0, 0.0248, 0.052, 0.0248, 0.052),
      (0.06472, 0.001953, 0.0568, 0.2067, -0.1518, 0.2919, -0.1518),
      (0.003081, -0.002352, 0.0001578, -0.000685, -0.0001649, -0.0004483, -0.0001649)
    )
    val Fm = DenseVector(123,219,124.8,129.6,23.52,21.6,91.74)
    val A = JR*diag(Fm)
    val v = DenseVector(0.0,1.0,0.0,0.0) //xy direction
    val OrthonormalBasis = Ortho(Basis(A)) //Orthogonalize the basis
    val AlphaLenOut = 10
    val PointsPerAlpha = 100
    val db = PointStream.alphaGenerate(PointsPerAlpha, Tuple2(0.0, 0.9),AlphaLenOut, v, A, OrthonormalBasis, RandomObject)
    db.cols should equal (12)
    db.rows should equal (1000)
    val FileName = Output.TimestampCSVName("output/Y_alphaProgression").toString()
    val MyFile = new java.io.File(FileName)
    csvwrite(MyFile, db)
  }
  "PointStreamXY" should "generate 100 points( per alpha) in a direction progression where alpha is increasing in the xy direction." in {
    import bbdl.space._
    import breeze.linalg._
    import breeze.numerics._
    import breeze.stats._
    val Seed = 10
    val RandomObject = new scala.util.Random(Seed)
    val JR = DenseMatrix(
      (-0.08941, -0.0447, 0.2087, -0.2138, -0.009249, 0.1421, 0.03669),
      (-0.04689, -0.1496, 0.0, 0.0248, 0.052, 0.0248, 0.052),
      (0.06472, 0.001953, 0.0568, 0.2067, -0.1518, 0.2919, -0.1518),
      (0.003081, -0.002352, 0.0001578, -0.000685, -0.0001649, -0.0004483, -0.0001649)
    )
    val Fm = DenseVector(123,219,124.8,129.6,23.52,21.6,91.74)
    val A = JR*diag(Fm)
    val v = DenseVector(1.0,1.0,0.0,0.0) //xy direction
    val OrthonormalBasis = Ortho(Basis(A)) //Orthogonalize the basis
    val AlphaLenOut = 10
    val PointsPerAlpha = 100
    val db = PointStream.alphaGenerate(PointsPerAlpha, Tuple2(0.0, 0.9),AlphaLenOut, v, A, OrthonormalBasis, RandomObject)
    db.cols should equal (12)
    db.rows should equal (1000)
    val FileName = Output.TimestampCSVName("output/XY_alphaProgression").toString()
    val MyFile = new java.io.File(FileName)
    csvwrite(MyFile, db)
  }
  "PointStream" should "generate the same result as a direct hit and run computation (given v)" in {
    import bbdl.space._
    import breeze.linalg._
    import breeze.numerics._
    import breeze.stats._
    val Seed = 10
    val RandomObject = new scala.util.Random(Seed)
    val JR = DenseMatrix(
      (-0.08941, -0.0447, 0.2087, -0.2138, -0.009249, 0.1421, 0.03669),
      (-0.04689, -0.1496, 0.0, 0.0248, 0.052, 0.0248, 0.052),
      (0.06472, 0.001953, 0.0568, 0.2067, -0.1518, 0.2919, -0.1518),
      (0.003081, -0.002352, 0.0001578, -0.000685, -0.0001649, -0.0004483, -0.0001649)
    )
    val Fm = DenseVector(123,219,124.8,129.6,23.52,21.6,91.74)
    val A = JR*diag(Fm)
    val v = DenseVector(2.865986, 2.865986, 2.865986, 0.0) //xy direction
    val OrthonormalBasis = Ortho(Basis(A)) //Orthogonalize the basis
    val DirectHitandRunPoints = PointStream.generate(1000,OrthonormalBasis,GenStartingPoint(A,v),RandomObject)
    val FileName = Output.TimestampCSVName("output/direct_points286").toString()
    val MyFile = new java.io.File(FileName)
    csvwrite(MyFile, DirectHitandRunPoints)
  }
  "PointStream" should "generate 1000 points( per alpha) WITH COSTS, per direction progression where alpha is increasing in the x direction." in {
    import bbdl.space._
    import breeze.linalg._
    import breeze.numerics._
    import breeze.stats._
    val Seed = 10
    val RandomObject = new scala.util.Random(Seed)
    val JR = DenseMatrix(
      (-0.08941, -0.0447, 0.2087, -0.2138, -0.009249, 0.1421, 0.03669),
      (-0.04689, -0.1496, 0.0, 0.0248, 0.052, 0.0248, 0.052),
      (0.06472, 0.001953, 0.0568, 0.2067, -0.1518, 0.2919, -0.1518),
      (0.003081, -0.002352, 0.0001578, -0.000685, -0.0001649, -0.0004483, -0.0001649)
    )
    val Fm = DenseVector(123,219,124.8,129.6,23.52,21.6,91.74)
    val A = JR*diag(Fm)
    val v = DenseVector(1.0,0.0,0.0,0.0) //xy direction
    val OrthonormalBasis = Ortho(Basis(A)) //Orthogonalize the basis
    val AlphaLenOut = 10
    val PointsPerAlpha = 1000
    val db = PointStream.alphaGenerate(PointsPerAlpha, Tuple2(0.0, 0.9), AlphaLenOut, v, A, OrthonormalBasis, RandomObject)
    val DBwithCosts = Cost.GenCosts(db, A.cols, Fm)
    db.cols should equal (12)
    db.rows should equal (10000)
    val FileName = Output.TimestampCSVName("output/X_alphaProgression").toString()
    val MyFile = new java.io.File(FileName)
    csvwrite(MyFile, DBwithCosts)
  }
}

class ExtrudeVectorSpec() extends FlatSpec with Matchers {
  "ExtrudeVector" should "Make the right size and shape of matrix" in {
    val v = DenseVector(1.0,1.0,2.0,0.0)
    val n = 2
    val res = ExtrudeVector(v,n)
    res should be (DenseMatrix((1.0,1.0,2.0,0.0),(1.0,1.0,2.0,0.0)))
  }
}

class VectorRepeatSpec() extends FlatSpec with Matchers {
  "VectorRepeat" should "turn a short vector into a repeated by-element vector" in {
    val v = DenseVector(0.2,0.4,0.6,0.8)
    val n = 1
    val res = VectorRepeat(v,n)
    res should be (v)
  }
  "VectorRepeat" should "turn a long vector into a repeated by-element vector" in {
    val v = DenseVector(0.2,0.4,0.6,0.8)
    val n = 2
    val res = VectorRepeat(v,n)
    res should be (DenseVector(0.2,0.2,0.4,0.4,0.6,0.6,0.8,0.8))
  }
}

class CostSpec() extends  FlatSpec with Matchers {
  behavior of "L1"
  val ExpV  = DenseVector(0.227166716,	0.110954506,	0.19195692,	0.134892534,	0.137534494,	0.805810655,	0.820020392)
  val ExpW = DenseVector(123,219,124.8,129.6,23.52,21.6,91.74)
  it should "return the simple sum of a vector of numbers" in {
    assert(Cost.L1Norm(DenseVector(1.0,2.0))==3.0)
    assert(Cost.L1Norm(DenseVector(1.0,2.0,3.0))==6.0)
    assert(Cost.L1Norm(DenseVector(1.0,8.0))==9.0)
  }
  "L2" should "get the pythagorean magnitude of the vector" in {
    assert(Cost.L2Norm(DenseVector(1.0,3.0,2.0))==sqrt(14))
  }
  "L3" should "multiply each element of the vector by its weight" in {
    assert(Cost.L3Norm(DenseVector(1.0,2.0,3.0))==cbrt(1.0+8.0+27.0))
  }
  "L1weighted" should "multiply each element of the vector by its weight" in {
    val v = DenseVector(1.0,2.0,3.0) //vector
    val w = DenseVector(2.0,2.0,1.0) //weightings array
    assert(Cost.L1WeightedNorm(v,w) == 9.0)
  }
  "L2weighted" should "multiply each element of the vector by its weight" in {
    val v = DenseVector(1.0,2.0,3.0) //vector
    val w = DenseVector(2.0,2.0,1.0) //weightings array
    assert(Cost.L2WeightedNorm(v,w) == sqrt(29.0))
  }
  "L3weighted" should "multiply each element of the vector by its weight" in {
    val v = DenseVector(1.0,2.0,3.0) //vector
    val w = DenseVector(2.0,2.0,1.0) //weightings array
    assert(Cost.L3WeightedNorm(v,w)== cbrt(99.0))
  }
  "L1-3 weighted and nonweighted" should "Work on experimental index finger data" in {
    val res = DenseVector(2.428336216,	1.208155232,	1.032237834,	189.547831,	90.68331237,	78.63872848)
    assert(Cost.L1Norm(ExpV)-res(0) < 1E-6)
    assert(Cost.L2Norm(ExpV)-res(1)< 1E-6)
    assert(Cost.L3Norm(ExpV)-res(2)< 1E-6)
    assert(Cost.L1WeightedNorm(ExpV,ExpW)- res(3)< 1E-6)
    assert(Cost.L2WeightedNorm(ExpV,ExpW)- res(4)< 1E-6)
    assert(Cost.L3WeightedNorm(ExpV,ExpW)- res(5)< 1E-6)
  }

}

class VectorExcursionSpec() extends FlatSpec with Matchers {
  behavior of "VectorExcursion"
  it should "return true when range exceeds i" in {
    val vector_of_interest = DenseVector(1.0,2.0,3.0,4.0,5.0)
    val is_outside_range = RangeExcursion.ValueOutsideRange(vector_of_interest,500)
    val string_version = is_outside_range.toString()
    string_version shouldBe "false"
  }

  it should "return false without string workaround" in {
    val vector_of_interest = DenseVector(1.0,2.0,3.0,4.0,5.0)
    val is_outside_range = RangeExcursion.ValueOutsideRange(vector_of_interest,500)
    is_outside_range shouldBe false
  }
}