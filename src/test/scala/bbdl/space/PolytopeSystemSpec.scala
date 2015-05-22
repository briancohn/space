package bbdl.space

import bbdl.space.SampleDataFunctions.PreExpansion._
import breeze.linalg.{DenseVector, DenseMatrix}
import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by B on 5/21/2015.
 * note- theo is theoretical (expected) and exp is experimental
 */
class PolytopeSystemsSpec() extends FlatSpec with Matchers{
  val A = SampleDataFunctions.PreExpansion.A1
  val b = SampleDataFunctions.PreExpansion.b1
  val deltas = SampleDataFunctions.PreExpansion.deltas1
  val MiniSystem1 = GeneratorSystem(A, b , deltas)
  "System Class" should "return its A when called" in {
    assert(MiniSystem1.A == A)
  }
  "System Class" should "return its b when called" in {
    assert(MiniSystem1.b == b)
  }
  "System Class" should "return its deltas when called" in {
    assert(MiniSystem1.deltas == deltas)
  }
}