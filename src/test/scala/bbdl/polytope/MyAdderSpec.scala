package bbdl.polytope
import breeze.linalg._
import breeze.numerics._
import org.scalatest._ //added for this test file

class MyAdderSpec extends FlatSpec with Matchers {
	behavior of "MyAdder"

	it should "Add two numbers" in {
	  val adder = new MyAdder()

	  val r = adder(1.0, 2.0)

	  r should be (3.0)
	}
}


class GetRandomDirectionSpec extends FlatSpec with Matchers {
	behavior of "GetRandomDirection"
	it should "Get a random direction for all positive inputs" in {
	val GetRandomDirection = new GetRandomDirection()
	  val B = DenseMatrix((1.0,0.0), (0.0,1.0), (0.0, 0.0))
	  val v = DenseVector(0.1,0.2)
	  val RandomDirection = GetRandomDirection(B,v)
	  val expected = DenseVector(0.1,0.2,0.0)
	  RandomDirection should be expected
	}
	it should "get a random direction for some negative inputs" in {
	  val B = DenseMatrix((-1.0,0.0), (0.0,1.0), (0.0, 0.0))
	  val v = DenseVector(0.1,-0.2)
	  val RandomDirection = GetRandomDirection(B,v)
	  val expected = DenseVector(-0.1, -0.2, 0.0)
	  RandomDirection should be expected
	}
}

class BasisSpec extends FlatSpec with Matchers {
	behavior of "GenerateBasis"
	it should "take in a matrix of size (4,2) (generators)" in {
	  val A = DenseMatrix((1.0,1.0,1.0,1.0), (2.0,1.0,1.0,1.0))
	  val basis = GenerateBasis(A)
	  val ExpectedBasis = DenseMatrix((0.0,0.0),(-1.0,-1.0), (1.0,0.0), (0.0, 1.0))
	  basis should be ExpectedBasis
	}
	it should "take in a matrix of size (2,4) generators" in {
	  val A = DenseMatrix((0.0,0.0,1.0,1.0,1.0), (1.0,1.0,1.0,0.0,1.0), (2.0,1.0,1.0,1.0,0.0))
	  val basis = GenerateBasis(A)
	  val ExpectedBasis = DenseMatrix((-1,1),(2, -1),(-1, -1),(1,0),(0,1))
	  basis should be ExpectedBasis
	}
}

class OrthoSpec extends FlatSpec with Matchers {
	behavior of "orthonormalize"
	it should "take in a matrix of size (3,3); the basis" in {
	  val A = DenseMatrix((1,2,5), (1,1,1), (1,0,3))
	  val BasisOrthonormal = Ortho(A)
	  val ExpectedBasisOrthonormal = DenseMatrix((1/sqrt(3), 1/sqrt(2), 1/sqrt(6)), (1/sqrt(3), 0, -sqrt(2)/sqrt(3)), (1/sqrt(3), 1/sqrt(2), 1/sqrt(6)))
	  BasisOrthonormal should be ExpectedBasisOrthonormal
	}
}

class GetEndpointsSpec extends FlatSpec with Matchers {
	behavior of "GetEndpoints"
	it should "take in two vectors; a point (3) and a direction (3)" in {
	  val p = DenseVector(0,0.5,0.5)
	  val q = DenseVector(2,1,2)
	  val endpoints = GetEndpoints(p,q)
	  val FirstEndpoint  = endpoints._1
	  val SecondEndpoint = endpoints._2
	  val ExpectedFirstPoint = DenseVector(0,0.5,0.5)
	  val ExpectedSecondPoint = DenseVector(0.5, 0.75, 1)
	  FirstEndpoint should be ExpectedFirstPoint
      SecondEndpoint should be ExpectedSecondPoint
	}
}


