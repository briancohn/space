package bbdl.polytope

import org.scalatest._

class MyAdderSpec extends FlatSpec with Matchers {
	behavior of "MyAdder"

	it should "Add two numbers" in {
	  val adder = new MyAdder()

	  val r = adder(1.0, 2.0)

	  r should be (3.0)
	}
}