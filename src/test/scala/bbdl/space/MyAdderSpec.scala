package bbdl.space

import java.io.File

import breeze.linalg._
import breeze.numerics._
import breeze.math._
import org.scalatest._
import Matchers._

import scala.util.Random

//added for this test file

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
    assert(FOutputVectorIndex === DenseVector(0.0, 6.757881713139706, 0.0, 0.0))//pasted output - test generated for sustainability tracking
  }
  it should "Get correct activation vector where y is maximized on the FVC index finger in 7D." in {
    assert(IndexAVec === DenseVector(0.14390467823812347, 0.0, 0.1724420617559178, 0.3293781199403045, 1.0, 1.0, 1.0))
  }
  it should "Generate force when using the a solution and the Fm matrix" in {
    val ExpF = AIndex*IndexAVec
    val AbsError = ElementwiseAbsoluteDifference(ExpF.toDenseMatrix, FOutputVectorIndex.toDenseMatrix)
    assert(AbsError < 1E-14)
  }
  it should "Get correct maximum force vector where z is maximized on the FVC index finger in 7d" in {
    val bIndex_z = DenseVector(0.0,0.0,1.0,0.0)
    val IndexMaxInfo_z = MaximumOutput(AIndex, bIndex_z)
    assert(IndexMaxInfo_z._1 == DenseVector(0.0, 0.0, 40.28419614785415, 0.0))
    assert(IndexMaxInfo_z._2 == DenseVector(0.27027228376106427, 0.05927684709966398, 1.0, 0.922552974363257, 0.0, 1.0, 0.0))
  }
  it should "Get correct maximum force vector where y and z is maximized on the FVC index finger in 7d" in {
    val bIndex_yz = DenseVector(0.0,1.0,1.0,0.0)
    val IndexMaxInfo_yz = MaximumOutput(AIndex, bIndex_yz)
    assert(IndexMaxInfo_yz._1 == DenseVector(0.0, 7.195388104572386, 7.195388104572386, 0.0))
    assert(IndexMaxInfo_yz._2 == DenseVector(0.17866959252521905, 0.0, 0.3982986123112096, 0.5278834439011518, 1.0, 1.0, 0.9999999999999999))
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
    assert(ElementwiseAbsoluteDifference(RandomDirection.toDenseMatrix, DenseVector(0.6685391145389219, 0.6327216675776454, -0.9424452549137063, -0.009898782433063143, 1.8021608700963798, 1.8765599210333714, 0.5111602993013169, 0.8418645608663314, -0.8955260348019763, -0.4938850345409942, 0.0).toDenseMatrix) < 1E-14)
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
    val AbsError = ElementwiseAbsoluteDifference(Orthobasis, ExpectedOrthoBasis)
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
    println(Endpoints)
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

class SampleLinearSystemSpec() extends FlatSpec with Matchers{
  behavior of "Sample Linear System"
  it should "take in a (1,3) matrix and output a set of 50 points using May's simple 3 variable example." in {
    val NumberToGenerate = 20
    val Seed = 10
    val RandomObject = new scala.util.Random(Seed)
    val A = DenseMatrix(
      (10.0/3.0, -53.0/15.0, 2.0)
    )
    val v = DenseVector(1.0)
    val solutions = SampleLinearSystem(A,v,RandomObject,NumberToGenerate)
    val expectedSolutions = DenseMatrix(
      (0.5331051701610331, 	0.7524377270512136, 	0.9407980341887554   ),
      (0.44279704205455184, 	0.3444940732667327, 	0.37061112601364143  ),
      (0.26875228352361274, 	0.25439227355582095, 	0.5015058774092624   ),
      (0.11511534912591137, 	0.07728892296784273, 	0.44468484870000335  ),
      (0.1855853677879575, 	0.1873927291818172, 	0.521751541907948    ),
      (0.5269493268380042, 	0.4863598454140634, 	0.4809868488348383   ),
      (0.48781857407151524, 	0.32258699016344317, 	0.2568727258362242   ),
      (0.8195848076010065, 	0.5559180110010715, 	0.11614714010021537  ),
      (0.586313872886115, 	0.5593802131705827, 	0.5110485884578376   ),
      (0.5692621666652598, 	0.37852341524562416, 	0.21995442249183628  ),
      (0.7682818796024014, 	0.6490392716982926, 	0.36616624732964764  ),
      (0.6671552196273187, 	0.6012638499417942, 	0.45030743551830493  ),
      (0.7110334512041154, 	0.6706667673202662, 	0.49978887025894414  ),
      (0.41506878818916493, 	0.3230120667630799, 	0.3788733376328327   ),
      (0.40131727898178654, 	0.25123996890112654, 	0.274995146755679    ),
      (0.41661899912685474, 	0.27506898793279144, 	0.29159021346983993  ),
      (0.3718091057776773, 	0.27693078755754513, 	0.36956254838886726  ),
      (0.3900685757439571, 	0.21350751863950776, 	0.22708232335653494  ),
      (0.034511957769654794, 	0.029094128868510573, 	0.4938796980516105   ),
      (0.3850201715024376, 	0.11398690398706118, 	0.05967657787307851  ))

    assert(solutions == expectedSolutions)
  }
//   behavior of "Sample Finger Linear System"
//   it should "take in a 7dim finger matrix model and output a set" in {
//     val NumberToGenerate = 250
//     val Seed = 10
//     val RandomObject = new scala.util.Random(Seed)
//     val JR = DenseMatrix(
//       (-0.08941, -0.0447, 0.2087, -0.2138, -0.009249, 0.1421, 0.03669),
//       (-0.04689, -0.1496, 0.0, 0.0248, 0.052, 0.0248, 0.052),
//       (0.06472, 0.001953, 0.0568, 0.2067, -0.1518, 0.2919, -0.1518),
//       (0.003081, -0.002352, 0.0001578, -0.000685, -0.0001649, -0.0004483, -0.0001649)
//     )
//     val Fm = DenseVector(123,219,124.8,129.6,23.52,21.6,91.74)
//     val A = JR*diag(Fm)
//     println(A)
//     val v = DenseVector(1.0,1.0,1.0,0.0)
//     val solutions = SampleLinearSystem(A,v,RandomObject,NumberToGenerate)
//     val MyFile = new File("7dof.csv")
//     csvwrite(MyFile,solutions)
//     //TODO add test
// }
//  behavior of "Sample Finger Linear System"
//  it should "take in a cat leg matrix model and output a set" in {
//    val NumberToGenerate = 20000
//    val Seed = 10
//    val RandomObject = new scala.util.Random(Seed)
//    val CatJ = DenseMatrix(
//      (0.2235,    0.0084,   -0.0147,    0.1471,   -0.0038,   -0.0706,   -0.0183),
//      (-0.0284,   -0.0096,   -0.0078,   -0.0901,   -0.0017,    0.0095,   -0.0090),
//      (    0.0,   -0.1494,   -0.1686,   -0.0158,   -0.0358,    0.0008,   -0.0743),
//      (    0.0,    0.7522,    0.6588,    0.1909,   -0.2869,   -0.0128,    0.9108),
//      (    0.0,    0.6590,   -0.7520,    0.1414,   -0.9549,   -0.0136,   -0.3723),
//      ( 1.0000,         0.0,   -0.0226,    0.9714,    0.0764,   -0.9998,   -0.178)
//    )
//    val CatFm = DenseVector(99.853, 11.062, 44.494, 160.94, 20.784, 19.524, 101.81, 11.747, 88.595, 4.052, 29.564, 95.845, 81.742, 32.657, 10.377, 15.794, 72.705, 118.93, 15.597, 25.443, 38.687, 118.45, 19.677, 75.673, 19.885, 86.344, 25.404, 38.435, 39.611, 136.92, 56.911)
////    val CatR = DenseMatrix(7, 31, List(-0.0278350,-0.0188290,-0.0286830,-0.0307170,0.0000000,0.0000000,0.0000000,-0.0020837,0.0003486,0.0014568,-0.0292740,0.0000000,0.0000000,0.0000000,-0.0092456,0.0000000,0.0000000,0.0039358,0.0000000,0.0008967,-0.0071435,0.0037420,0.0228300,-0.0276120,0.0000000,-0.0358220,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,0.0039910,0.0066489,-0.0006356,-0.0015485,0.0000000,0.0000000,0.0000000,-0.0028456,-0.0071797,-0.0052183,0.0058436,0.0000000,0.0000000,0.0000000,0.0017835,0.0000000,0.0000000,0.0015483,0.0000000,-0.0088983,-0.0040605,-0.0012275,0.0149200,0.0010194,0.0000000,-0.0026231,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,0.0031579,0.0006709,0.0021240,0.0042832,0.0000000,0.0000000,0.0000000,-0.0086436,-0.0150550,-0.0155080,0.0016880,0.0000000,0.0000000,0.0000000,0.0003057,0.0000000,0.0000000,-0.0012821,0.0000000,0.0011985,0.0077211,0.0003367,-0.0010192,-0.0018145,0.0000000,-0.0028371,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,-0.0338020,0.0021471,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,-0.0281250,-0.0079683,-0.0087790,0.0000000,0.0000000,0.0000000,-0.0085280,0.0000000,0.0000000,0.0000000,0.0000000,0.0097753,0.0000000,-0.0043720,0.0000000,-0.0342250,0.0000000,0.0000000,0.0091027,0.0088521,0.0089962,0.0000000,0.0000000,0.0000000,0.0124140,-0.0054230,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,-0.0014051,-0.0041060,0.0011166,0.0000000,0.0000000,0.0000000,-0.0051443,0.0000000,0.0000000,0.0000000,0.0000000,-0.0047712,0.0000000,-0.0051087,0.0000000,-0.0066722,0.0000000,0.0000000,-0.0032677,-0.0019441,-0.0071063,0.0000000,0.0000000,0.0000000,0.0000000,-0.0097320,0.0015661,0.0043282,0.0000000,0.0000000,0.0000000,0.0000000,0.0096824,0.0114020,0.0011901,0.0000000,0.0007996,0.0145900,0.0000000,0.0005580,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,0.0106080,0.0000000,-0.0086770,0.0002877,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,-0.0033325,0.0099560,0.0014359,0.0000000,0.0000000,0.0000000,0.0000000,0.0044028,0.0028491,-0.0022826,0.0000000,-0.0059687,0.0050729,0.0000000,-0.0028017,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,0.0043114,0.0000000,-0.0019022,0.0097350,0.0000000,0.0000000,0.0000000))
//
//    val A = CatJ*CatR*diag(CatFm)
//    println(A)
//    val v = DenseVector(1.0,1.0,1.0,0.0,0.0,0.0)
//    val solutions = SampleLinearSystem(A,v,RandomObject,NumberToGenerate)
//    val MyFile = new File("7dof.csv")
//    csvwrite(MyFile,solutions)
    //    val expectedSolutions = DenseMatrix(
    //      (0.5331051701610331, 	0.7524377270512136, 	0.9407980341887554   ),
    //      (0.44279704205455184, 	0.3444940732667327, 	0.37061112601364143  ),
    //      (0.26875228352361274, 	0.25439227355582095, 	0.5015058774092624   ),
    //      (0.11511534912591137, 	0.07728892296784273, 	0.44468484870000335  ),
    //      (0.1855853677879575, 	0.1873927291818172, 	0.521751541907948    ),
    //      (0.5269493268380042, 	0.4863598454140634, 	0.4809868488348383   ),
    //      (0.48781857407151524, 	0.32258699016344317, 	0.2568727258362242   ),
    //      (0.8195848076010065, 	0.5559180110010715, 	0.11614714010021537  ),
    //      (0.586313872886115, 	0.5593802131705827, 	0.5110485884578376   ),
    //      (0.5692621666652598, 	0.37852341524562416, 	0.21995442249183628  ),
    //      (0.7682818796024014, 	0.6490392716982926, 	0.36616624732964764  ),
    //      (0.6671552196273187, 	0.6012638499417942, 	0.45030743551830493  ),
    //      (0.7110334512041154, 	0.6706667673202662, 	0.49978887025894414  ),
    //      (0.41506878818916493, 	0.3230120667630799, 	0.3788733376328327   ),
    //      (0.40131727898178654, 	0.25123996890112654, 	0.274995146755679    ),
    //      (0.41661899912685474, 	0.27506898793279144, 	0.29159021346983993  ),
    //      (0.3718091057776773, 	0.27693078755754513, 	0.36956254838886726  ),
    //      (0.3900685757439571, 	0.21350751863950776, 	0.22708232335653494  ),
    //      (0.034511957769654794, 	0.029094128868510573, 	0.4938796980516105   ),
    //      (0.3850201715024376, 	0.11398690398706118, 	0.05967657787307851  ))
    //
    //    assert(solutions == expectedSolutions)
//  }
}

class UpdateMeanSpec() extends FlatSpec with Matchers{
  behavior of "UpdateMean"
  it should "take in a prior mean (and an n) and update the running mean with a new value" in {
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
//  it should "stream until the predicate is met" in {
//    import bbdl.space._
//    import breeze.linalg._
//    import breeze.numerics._
//    import breeze.stats._
//    val Seed = 10
//    val RandomObject = new scala.util.Random(Seed)
//    val A = DenseMatrix(
//      (10.0/3.0, -53.0/15.0, 2.0)
//    )
//    val v = DenseVector(1.0)
//    val OrthonormalBasis = Ortho(Basis(A)) //Orthogonalize the basis
//    var CurrentPoint = GenStartingPoint(A, v)
//    val res  = PointStream.start(OrthonormalBasis, CurrentPoint, RandomObject, PointStream.HardCodedStop)
//    println(res)
//  }
//  "PointStream" should "stream until the predicate is met for the 7dim finger model" in {
//    import bbdl.space._
//    import breeze.linalg._
//    import breeze.numerics._
//    import breeze.stats._
//    val Seed = 10
//    val RandomObject = new scala.util.Random(Seed)
//    val JR = DenseMatrix(
//      (-0.08941, -0.0447, 0.2087, -0.2138, -0.009249, 0.1421, 0.03669),
//      (-0.04689, -0.1496, 0.0, 0.0248, 0.052, 0.0248, 0.052),
//      (0.06472, 0.001953, 0.0568, 0.2067, -0.1518, 0.2919, -0.1518),
//      (0.003081, -0.002352, 0.0001578, -0.000685, -0.0001649, -0.0004483, -0.0001649)
//    )
//    val Fm = DenseVector(123,219,124.8,129.6,23.52,21.6,91.74)
//    val A = JR*diag(Fm)
//    println(A)
//    val v = DenseVector(1.0,1.0,1.0,0.0)
//
//
//    val Upperbounds = Bounds.ComputeUppers(A,v)
//    val Lowerbounds = Bounds.ComputeLowers(A,v)
//    println(Upperbounds)
//    println(Lowerbounds)
//
//    val OrthonormalBasis = Ortho(Basis(A)) //Orthogonalize the basis
//    var CurrentPoint = GenStartingPoint(A, v)
//    val res  = PointStream.start(OrthonormalBasis, CurrentPoint, RandomObject, PointStream.HardCodedStop)
//    println(res)
//  }
//  "PointStream" should "stream until the predicate is met for the cat hindlimb model" in {
//    import bbdl.space._
//    import breeze.linalg._
//    import breeze.numerics._
//    import breeze.stats._
//    val Seed = 10
//    val RandomObject = new scala.util.Random(Seed)
//    val CatAMatrix = Array(Array(-0.62250,-0.04600,-0.28690,-1.92510,0.02250,-0.00570,-0.03380,-0.00430,0.02110,0.00210,-0.31490,-0.18410,-0.17600,-0.00140,-0.02130,0.00080,-0.17140,0.10840,0.00020,0.00270,-0.06750,0.26980,0.10320,-0.51150,-0.01650,-1.12220,0.01640,-0.00760,0.05350,0.17940,0.07690),
//      Array(0.07250,0.00510,0.03570,0.62410,-0.00510,-0.00150,0.00290,0.00180,0.01560,0.00050,0.09750,0.07460,0.07130,0.00100,0.00250,0.00100,0.06330,-0.01390,0.00050,0.00130,0.00700,-0.11490,-0.01540,0.09010,0.00120,0.35920,-0.00170,-0.00330,-0.03230,-0.10880,-0.04550),
//      Array(-0.11270,-0.01220,-0.01170,-0.06460,0.00830,-0.01440,-0.01050,0.02210,0.31990,0.01380,-0.01960,-0.00450,-0.00850,0.00560,-0.00330,0.00700,-0.00340,-0.00180,0.00330,0.02870,-0.02690,0.01700,-0.04050,0.03070,-0.00620,0.14250,0.00340,-0.02780,-0.00110,-0.00960,0.00640),
//      Array(0.50750,0.06020,0.04100,-1.34510,-0.01960,0.17670,0.12750,-0.09200,-1.35720,-0.05730,0.01600,0.33950,0.03700,-0.06840,0.01600,-0.08600,0.31120,0.03800,-0.03990,-0.15020,0.07860,0.30010,0.20760,0.01530,0.07540,-0.73070,-0.04120,0.34070,0.10600,0.30780,0.21380),
//      Array(0.02550,0.04290,-0.08970,-3.35960,0.14250,-0.07280,-0.06040,0.05430,0.58380,0.03330,-0.00160,0.09810,-0.28800,0.02720,0.00980,0.03490,0.11780,0.23600,0.01610,-0.17210,-0.32810,0.57760,0.20850,0.47650,-0.03480,0.16720,0.02100,-0.13940,0.17460,0.42560,0.45860),
//      Array(-2.78650,-0.20850,-1.27840,-10.09050,0.24930,-0.06530,-0.46660,-0.02220,0.06110,0.00730,-1.67750,-1.77510,-1.66350,-0.02560,-0.09600,0.00420,-1.75730,0.47150,-0.00090,0.02210,-0.28310,1.52380,0.44970,-2.43730,-0.22620,-6.00200,0.22900,-0.07780,0.34040,1.15700,0.46640))
//    val v = DenseVector(10.0,10.0,10.0,0.0,0.0,0.0)
//    val A = breeze.util.JavaArrayOps.array2DToDm(CatAMatrix)
//    val Upperbounds = Bounds.ComputeUppers(A,v)
//    val Lowerbounds = Bounds.ComputeLowers(A,v)
//
//    val OrthonormalBasis = Ortho(Basis(A)) //Orthogonalize the basis
//    var CurrentPoint = GenStartingPoint(A, v)
////    val res  = PointStream.start(OrthonormalBasis, CurrentPoint, RandomObject, PointStream.HardCodedStop)
////    val res1 = PointStream.iteratorApproach(OrthonormalBasis, CurrentPoint, RandomObject)
//    val res1 = PointStream.iteratorApproach(OrthonormalBasis, CurrentPoint, RandomObject).take(1000).toList
//    println(res1)
//  }
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
    println("Saving to " + FileName)
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
    println(db(::,11))
  }
  "PointStream" should "generate 10m points( per alpha) in a direction progression where alpha is increasing in the xyz direction." in {
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
    val AlphaLenOut = 10
    val PointsPerAlpha = 10

    val db = PointStream.alphaGenerate(PointsPerAlpha, Tuple2(0.0, 0.9),AlphaLenOut, v, A, OrthonormalBasis, RandomObject)
    db.cols should equal (12)
    db.rows should equal (100)
    val FileName = Output.TimestampCSVName("output/XYZ_alphaProgression").toString()
    val MyFile = new java.io.File(FileName)
    println("Saving to " + FileName)
    csvwrite(MyFile, db)
  }
  "PointStream" should "generate 10m points( per alpha) in a direction progression where alpha is increasing in the x direction." in {
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
    val PointsPerAlpha = 1000000
    val db = PointStream.alphaGenerate(PointsPerAlpha, Tuple2(0.0, 0.9),AlphaLenOut, v, A, OrthonormalBasis, RandomObject)
    db.cols should equal (12)
    db.rows should equal (100000000)
    val FileName = Output.TimestampCSVName("output/X_alphaProgression").toString()
    val MyFile = new java.io.File(FileName)
    println("Saving to " + FileName)
    csvwrite(MyFile, db)
  }
  "PointStream" should "generate 10m points( per alpha) in a direction progression where alpha is increasing in the y direction." in {
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
    val PointsPerAlpha = 1000000
    val db = PointStream.alphaGenerate(PointsPerAlpha, Tuple2(0.0, 0.9),AlphaLenOut, v, A, OrthonormalBasis, RandomObject)
    db.cols should equal (12)
    db.rows should equal (100000000)
    val FileName = Output.TimestampCSVName("output/Y_alphaProgression").toString()
    val MyFile = new java.io.File(FileName)
    println("Saving to " + FileName)
    csvwrite(MyFile, db)
  }
  "PointStreamXY" should "generate 10m points( per alpha) in a direction progression where alpha is increasing in the xy direction." in {
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
    val PointsPerAlpha = 1000000
    val db = PointStream.alphaGenerate(PointsPerAlpha, Tuple2(0.0, 0.9),AlphaLenOut, v, A, OrthonormalBasis, RandomObject)
    db.cols should equal (12)
    db.rows should equal (10000000)
    val FileName = Output.TimestampCSVName("output/XY_alphaProgression").toString()
    val MyFile = new java.io.File(FileName)
    println("Saving to " + FileName)
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
    println("Saving to " + FileName)
    csvwrite(MyFile, DirectHitandRunPoints)
  }
  "PointStream" should "generate 1000 points( per alpha) WITH COSTS in a direction progression where alpha is increasing in the x direction." in {
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
    var CostDB = new Array[Array[Double]](db.rows)
    for (i <- 0 to db.rows-1) {
      var RowI = db(i,::)
      CostDB(i) = Cost.CostVec(RowI.inner.toDenseVector.slice(0,A.cols), Fm).toArray
    }
    val DBwithCosts = DenseMatrix.horzcat(db,breeze.util.JavaArrayOps.array2ToDm(CostDB))

    db.cols should equal (12)
    db.rows should equal (10000)


    val FileName = Output.TimestampCSVName("output/X_alphaProgression").toString()
    val MyFile = new java.io.File(FileName)
    println("Saving to " + FileName)
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