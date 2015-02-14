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





class GetEndpointsSpec extends FlatSpec with Matchers {
	behavior of "GetEndpoints"
	it should "take in two vectors; a point (3) and a direction (3)" in {
	  val p = DenseVector(0.0,0.5,0.5)
	  val q = DenseVector(2.0,1.0,2.0)
	  val endpoints = GetEndpoints(p,q)
	  val FirstEndpoint  = endpoints._1
	  val SecondEndpoint = endpoints._2
	  val ExpectedFirstPoint = DenseVector(0,0.5,0.5)
	  val ExpectedSecondPoint = DenseVector(0.5, 0.75, 1)
    assert(FirstEndpoint == ExpectedFirstPoint & SecondEndpoint == ExpectedSecondPoint)
	}
}


