package bbdl.polytope
import org.scalatest._
import breeze.linalg._
import breeze.numerics._
class MyAdderSpec extends FlatSpec with Matchers {
	behavior of "MyAdder"

	it should "Add two numbers" in {
	  val adder = new MyAdder()

	  val r = adder(1.0, 2.0)

	  r should be (3.0)
	}
}


class get_random_direction_spec extends FlatSpec with Matchers {
	behavior of "get_random_direction"
	it should "Get a random direction for all positive inputs" in {
	val get_random_direction = new get_random_direction()
	  val B = DenseMatrix((1.0,0.0), (0.0,1.0), (0.0, 0.0))
	  val v = DenseVector(0.1,0.2)
	  val random_direction = get_random_direction(B,v)
	  val expected = DenseVector(0.1,0.2,0.0)
	  random_direction should be expected
	}
	it should "get a random direction for some negative inputs" in {
	  val B = DenseMatrix((-1.0,0.0), (0.0,1.0), (0.0, 0.0))
	  val v = DenseVector(0.1,-0.2)
	  val random_direction = get_random_direction(B,v)
	  val expected = DenseVector(-0.1, -0.2, 0.0)
	  random_direction should be expected
	}
}

class basis_spec extends FlatSpec with Matchers {
	behavior of "generate_basis"
	it should "take in a matrix of size (4,2) (generators)" in {
	  val A = DenseMatrix((1.0,1.0,1.0,1.0), (2.0,1.0,1.0,1.0))
	  val basis = generate_basis(A)
	  val expected_basis = DenseMatrix((0.0,0.0),(-1.0,-1.0), (1.0,0.0), (0.0, 1.0))
	  basis should be expected_basis
	}
	it should "take in a matrix of size (2,4) generators" in {
	  val A = DenseMatrix((0.0,0.0,1.0,1.0,1.0), (1.0,1.0,1.0,0.0,1.0), (2.0,1.0,1.0,1.0,0.0))
	  val basis = generate_basis(A)
	  val expected_basis = DenseMatrix((-1,1),(2, -1),(-1, -1),(1,0),(0,1))
	  basis should be expected_basis
	}
}


class orthonormalize_spec extends FlatSpec with Matchers {
	behavior of "orthonormalize"
	it should "take in a matrix of size (3,3); the basis" in {
	  val A = DenseMatrix((1,2,5), (1,1,1), (1,0,3))
	  val basis_orthonormal = orthonormalize(A)
	  val expected_basis_orthonormal = DenseMatrix((1/sqrt(3), 1/sqrt(2), 1/sqrt(6)), (1/sqrt(3), 0, -sqrt(2)/sqrt(3)), (1/sqrt(3), 1/sqrt(2), 1/sqrt(6)))
	  basis should be expected_basis
	}
}

class get_endpoints_spec extends FlatSpec with Matchers {
	behavior of "get_endpoints"
	it should "take in two vectors; a point (3) and a direction (3)" in {
	  val p = DenseVector(0,0.5,0.5)
	  val q = DenseVector(2,1,2)
	  val endpoints = get_endpoints(p,q)
	  val first_endpoint  = endpoints._1
	  val second_endpoint = endpoints._2
	  val exp_first_endpoint = DenseVector(0,0.5,0.5)
	  val exp_second_endpoint = DenseVector(0.5, 0.75, 1)
	  first_endpoint should be expected_endpoints
      second_endpoint
	}
}


