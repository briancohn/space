package bbdl.space
import breeze.linalg._
import breeze.numerics._
import breeze.math._
import org.scalatest._ //added for this test file

class GetRandomDirectionSpec extends FlatSpec with Matchers {
	behavior of "GetRandomDirection"

  it should "Get a random direction for all positive inputs" in {
    val B = DenseMatrix((1.0,0.0), (0.0,1.0), (0.0, 0.0))
    val v = DenseVector(0.1,0.2)
    val seed = 10
    val result = GetRandomDirection(B, seed)
    assert(result == DenseVector(0.0,0.8746788966462123,0.0))
	}
	it should "get a random direction for some negative inputs" in {
	  val B = DenseMatrix((-1.0,0.0), (0.0,1.0), (0.0, 0.0))
	  val v = DenseVector(0.1,-0.2)
    val seed = 10
	  val RandomDirection = GetRandomDirection(B, seed)
	  val expected = DenseVector(0.0, 0.8746788966462123, 0.0)
    assert(RandomDirection == expected)
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





// class GetEndpointsSpec extends FlatSpec with Matchers {
//   behavior of "Get Endpoints"
//   it should ""
	// behavior of "GetEndpoints"
	// it should "take in two vectors; a point (3) and a direction (3)" in {
	//   val p = DenseVector(0.0,0.5,0.5)
	//   val q = DenseVector(2.0,1.0,2.0)
	//   val endpoints = GetEndpoints(p,q)
	//   val FirstEndpoint  = endpoints._1
	//   val SecondEndpoint = endpoints._2
	//   val ExpectedFirstPoint = DenseVector(0,0.5,0.5)
	//   val ExpectedSecondPoint = DenseVector(0.5, 0.75, 1)
 //    assert(FirstEndpoint == ExpectedFirstPoint & SecondEndpoint == ExpectedSecondPoint)
 //    }
// }


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

