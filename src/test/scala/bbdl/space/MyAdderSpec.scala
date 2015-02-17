package bbdl.space
import breeze.linalg._
import breeze.numerics._
import breeze.math._
import org.scalatest._ //added for this test file

class GetRandomDirectionSpec extends FlatSpec with Matchers {
	behavior of "GetRandomDirection"

  it should "Get a random direction for all positive inputs" in {
    val B = DenseMatrix((1.0,0.0), (0.0,1.0), (0.0, 0.0))
    val seed = 10
    val result = GetRandomDirection(B, seed)
    assert(result === DenseVector(0.8746788966462123, -0.9193443348656242, 0.0))
	}
	it should "get a random direction for some negative inputs" in {
	  val B = DenseMatrix((-1.0,0.0), (0.0,1.0), (0.0, 0.0))
    val seed = 10
	  val RandomDirection = GetRandomDirection(B, seed)
	  val expected = DenseVector(-0.8746788966462123, -0.9193443348656242, 0.0)
    assert(RandomDirection === expected)
	}
  it should "take in an identity matrix, and output a vector whose values are distributed gauss-normally" in{
    val n = 10
    val Seed = 10
    val IdentityMatrix = DenseMatrix.eye[Double](n)
    val RandomDirection = GetRandomDirection(IdentityMatrix,Seed)
    assert(RandomDirection === DenseVector(0.8746788966462123, -0.9193443348656242, 1.1329921492850181, -0.4598256884904446, 0.7338090601305023, 0.4427877081455783, -0.23610812307264825, 0.4619622460418042, -0.19304784617121043, -0.02331125940413))
  }
  it should "take in an identity matrix with an extra row of zeros at the bottom, and output a vector whose values are distributed gauss-normally" in{
    val n = 10
    val Seed = 10
    val IdentityMatrix = DenseMatrix.eye[Double](n)
    val TallerMatrix = DenseMatrix.vertcat(IdentityMatrix, DenseMatrix.zeros[Double](1,n))
    val RandomDirection = GetRandomDirection(TallerMatrix,Seed)
    assert(RandomDirection == DenseVector(0.8746788966462123, -0.9193443348656242, 1.1329921492850181, -0.4598256884904446, 0.7338090601305023, 0.4427877081455783, -0.23610812307264825, 0.4619622460418042, -0.19304784617121043, -0.02331125940413, 0.0))
  }


  it should "produce a gauss-distribution with mean 0 and standard deviation 1 on an identity matrix" in {
    val x = GetRandomDirection(breeze.linalg.DenseMatrix.eye[Double](10), 7)
    assert(x === DenseVector(0.8452060657049847, 0.9128761787534405, -0.2870786364749953, 0.7518594314874758, 1.335473668231534, -0.9499789372646104, 0.599049892177836, 1.204570743449295, 2.4820093995603614, -0.7539501059617072))
  }
  import bbdl.space.GetRandomDirection
  import breeze.linalg.DenseMatrix
  val n = 1000
  val Seed = 10
  val Mat = DenseMatrix.eye[Double](n)
  val Direction = GetRandomDirection(Mat, Seed)
  val Mu = breeze.stats.mean(Direction)
  val Sigma = breeze.stats.stddev(Direction)

  it should "produce a gaussian distribution with mean 0 with a very large matrix" in {
    assert(abs(Mu) < 0.10)
  }
  it should "produce a gaussian distribution with SD 1 with a very large matrix" in {
    assert(abs(Sigma - 1.0) < 0.05)
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
}

class OrthoSpec extends FlatSpec with Matchers {
	behavior of "orthonormalize"
	it should "take in a matrix of size (3,3); the basis" in {
    val BasisOrthonormal = Ortho( DenseMatrix((1.0,1.0),(0.0,1.0)) )
	  val ExpectedBasisOrthonormal = DenseMatrix((1.0,0.0), (0.0,1.0))
	  assert(BasisOrthonormal === ExpectedBasisOrthonormal)

	}
}


class GetEndpointsSpec extends FlatSpec with Matchers {
  val Seed = 10
  "GetEndpoints" should "Get endpoints for a point and a positive direction" in {
    val p = DenseVector(0.5,0.5,0.5)
    val q = DenseVector(-1.0,-2.0,1.0)
    val Endpoints = GetEndpoints(p,q, Seed)
    val ExpectedEndpoints = (DenseVector(0.75,1.0,0.25), DenseVector(0.25,0.0,0.75))
    assert(Endpoints === ExpectedEndpoints)
  }

  "GetEndpoints" should "Get endpoints for a point and a negative direction" in {
    val p = DenseVector(0.0,0.5,0.5)
    val q = DenseVector(2.0,1.0,2.0)
    val Endpoints = GetEndpoints(p,q, Seed)
    val ExpectedEndpoints = (DenseVector(0.0,0.5,0.5), DenseVector(0.5,0.75,1.0))
    assert(Endpoints === ExpectedEndpoints)
  }

  behavior of "UpperboundVal"
  it should "take in a 3d point and positive 3d direction, and output a 3d upperbound vector" in {
    val p = DenseVector(0.0,0.5,0.5)
    val q = DenseVector(2.0,1.0,2.0)
    val UpperBounds = GetEndpoints.GetUpperBoundVector(p,q)
    val ExpectedUpperBounds = DenseVector(0.5,0.5,0.25)
    assert(UpperBounds === ExpectedUpperBounds)
  }
  behavior of "LowerboundVal"
  it should "take in a 3d point and positive 3d direction, and output a 3d LowerBound vector" in {
    val p = DenseVector(0.0,0.5,0.5)
    val q = DenseVector(2.0,1.0,2.0)
    val UpperBounds = GetEndpoints.GetLowerBoundVector(p,q)
    val ExpectedUpperBounds = DenseVector(0.0,-0.5,-0.25)
    assert(UpperBounds === ExpectedUpperBounds)
  }
  behavior of "UpperboundVal"
  it should "take in a 3d point and negative 3d direction, and output a 3d upperbound vector" in {
    val p = DenseVector(0.5,0.5,0.5)
    val q = DenseVector(-1.0,-2.0,1.0)
    val UpperBounds = GetEndpoints.GetUpperBoundVector(p,q)
    val ExpectedUpperBounds = DenseVector(0.5,0.25,0.5)
    assert(UpperBounds === ExpectedUpperBounds)
  }
  behavior of "LowerboundVal"
  it should "take in a 3d point and negative 3d direction, and output a 3d LowerBound vector" in {
    val p = DenseVector(0.5,0.5,0.5)
    val q = DenseVector(-1.0,-2.0,1.0)
    val UpperBounds = GetEndpoints.GetLowerBoundVector(p,q)
    val ExpectedUpperBounds = DenseVector(-0.5,-0.25,-0.5)
    assert(UpperBounds === ExpectedUpperBounds)
    assert(UpperBounds === ExpectedUpperBounds)
    assert(UpperBounds === ExpectedUpperBounds)
    assert(UpperBounds === ExpectedUpperBounds)
  }
  //Set up a test for upper and lower bounds

  val Lowers = DenseVector(-1.0,-6.0,-4.0,-5.0,-3.0,-5.0)
  val Uppers = DenseVector(8.0,7.0,9.0,8.0,9.0,10.0)
  val PositiveBounds = GetEndpoints.GetBoundLimits(Uppers, Lowers)
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
    val Endpoints = GetEndpoints.FindEndpoints(p,q, UpperBoundInner,LowerBoundInner)
    val ExpectedEndpoints = (DenseVector(0.75,1.0,0.25), DenseVector(0.25,0.0,0.75))
    println(Endpoints)
    assert(Endpoints === ExpectedEndpoints)
  }
  it should "use the p, positive direction q, and bound information to assemble two endpoints" in {
    val p = DenseVector(0.0,0.5,0.5)
    val q = DenseVector(2.0,1.0,2.0)
    val LowerBoundInner = 0.0
    val UpperBoundInner = 0.25
    val Endpoints = GetEndpoints.FindEndpoints(p,q, UpperBoundInner,LowerBoundInner)
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

    val AExpected = DenseMatrix((1.0,2.0,3.0,0.0,0.0,0.0),
                                (4.0,5.0,6.0,0.0,0.0,0.0),
                                (-1.0,-2.0,-3.0,0.0,0.0,0.0),
                                (-4.0,-5.0,-6.0,0.0,0.0,0.0),
                                (-1.0,0.0,0.0,1.0,0.0,0.0),
                                (0.0,-1.0,0.0,0.0,1.0,0.0),
                                (0.0,0.0,-1.0,0.0,0.0,1.0),
                                (1.0,0.0,0.0,1.0,0.0,0.0),
                                (0.0,1.0,0.0,0.0,1.0,0.0),
                                (0.0,0.0,1.0,0.0,0.0,1.0))
    val myA = GenStartingPoint.ExpandAMatrix(A)
    assert(myA == AExpected)
  }
  behavior of "ExpandbVector"
  it should "take in a len 2 vector and output a len 10 vector expanded" in {
    val A = DenseMatrix((1.0,2.0,3.0),(4.0,5.0,6.0))
    val ColNum = A.cols
    val b = DenseVector(7.0,8.0)
    val ExpandedbVector = GenStartingPoint.ExpandbVector(b, ColNum)
    val Expectedb= DenseVector(7.0,8.0,-7.0,-8.0,0.0,0.0,0.0,1.0,1.0,1.0)
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
  it should "Take in two points and return a point on the line between them. An example with a zero in both" in {
    val E1 = DenseVector(0.75,0.5,0)
    val E2 = DenseVector(0.6,0,0.2)
    val MyPoint = RandomPointBetween(E1,E2,seed=7)
    val ExpectedPoint = DenseVector(0.6403951436909937, 0.13465047896997895, 0.14613980841200844)
    assert(MyPoint === ExpectedPoint)
  }
  "Randpt" should "Take in two points and return a point on the line between them. Example with no zero" in {
    val E1 = DenseVector(0.5,0.2,0.7)
    val E2 = DenseVector(0.1,0.8,0.9)
    val MyPoint = RandomPointBetween(E1,E2,seed=7)
    val ExpectedPoint = DenseVector(0.20772038317598313, 0.6384194252360254, 0.8461398084120084)
    assert(MyPoint === ExpectedPoint)
  }
  "Randpt" should "Take in two points and return a point on the line between them. Example with ones" in {
    val E1 = DenseVector(1.0,0.2,0.7)
    val E2 = DenseVector(0.1,0.8,1.0)
    val MyPoint = RandomPointBetween(E1,E2,seed=7)
    val ExpectedPoint = DenseVector(0.34237086214596213, 0.6384194252360254, 0.9192097126180127)
    assert(MyPoint === ExpectedPoint)
  }
  
}










