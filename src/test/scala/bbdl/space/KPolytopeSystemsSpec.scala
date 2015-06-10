package bbdl.space
import bbdl.space._
import breeze.linalg.{DenseVector, DenseMatrix}
import org.scalatest.{Matchers, FlatSpec}
import bbdl.space.SampleDataFunctions.PreExpansion.{A1,A2,A3,b1,b2,b3,deltas1,deltas2,deltas3}

/**
*
Created by B
Time: 16:18 PM, 5/21/2015;
Project: space
*/
//class KSystemCheckPointSpec() extends FlatSpec with Matchers{
//  "KSystemCheckPoint" should "say false for k3n2 out-of-space point" in {
//    val Kmini = SampleDataFunctions.K4N2Mini.KSystemsMini
//    val j = 2 // the last element in the ksystem
//    val InSet = KSystemCheckPoint(Kmini,DenseVector(1.0,1.0),j)
//    assert(!InSet)
//  }
//}
class KSystemConstraintsSpec() extends FlatSpec with Matchers{
  behavior of "PadWithZeroMatrices"
  it should "add the correct padding of zeros when it's the first (leftmost) System" in {
    assert(AbsDiff.Matricies(KSystemConstraints.PadWithZeroMatrices(A1, 0, 3), SampleDataFunctions.PostExpansion.paddedA1) < 1E-14)
  }
  it should "add the correct padding of zeros when it's the middle (non edge) System" in {
    val StackedA = DenseMatrix.vertcat(A2,-A2)
    val exp = KSystemConstraints.PadWithZeroMatrices(StackedA, 1, 3)
    val theoretical = SampleDataFunctions.PostExpansion.paddedA2
    assert(AbsDiff.Matricies(theoretical, exp)  < 1E-14)
  }
  it should "add the correct padding of zeros when it's the last (rightmost) System" in {
    assert(AbsDiff.Matricies(KSystemConstraints.PadWithZeroMatrices(A3, 2, 3), SampleDataFunctions.PostExpansion.paddedA3)  < 1E-14)
  }
  behavior of "PadWithZeroMatrices"
  val Vec = DenseVector(1.0,0.0,-1.0)

  it should "add  correct padding of zeros when it's the first (leftmost) System" in {
    assert(AbsDiff.Vectors(KSystemConstraints.PadWithZeroVector(Vec, 0, 6), DenseVector(1.0,0.0,-1.0,0.0,0.0,0.0)) < 1E-14)
  }
  it should "add  correct padding of zeros when it's the middle (non edge) System" in {
    val theo = DenseVector(0.0,1.0,0.0,-1.0,0.0,0.0)
    val exp = KSystemConstraints.PadWithZeroVector(Vec, 1, 6)
    assert(AbsDiff.Vectors(theo,exp) < 1E-14)
  }
  it should "add  correct padding of zeros when it's the last (rightmost) System" in {
    assert(AbsDiff.Vectors(KSystemConstraints.PadWithZeroVector(Vec, 3, 6), DenseVector(0.0,0.0,0.0,1.0,0.0,-1.0)) < 1E-14)
  }
  behavior of "Constraints for K System"
  val KInputSystem = SampleDataFunctions.PreExpansion.KSystemsMini
  it should "construct a matrix for an A=2,2 K=3 small example" in {
    val theo = SampleDataFunctions.PostExpansion.StepDeltaChangeConstraint
    val Experimental = KSystemConstraints.deltaConstraintsA(n=2, K=3)
    assert(AbsDiff.Matricies(theo,Experimental) < 1E-14)
  }
  it should "construct a vector of solutions for an A=2,2 K=3 small example" in {
    val theo = SampleDataFunctions.PostExpansion.DeltaConstraintB
    val Experimental = KSystemConstraints.deltaConstraintsb(KInputSystem)
    assert(AbsDiff.Vectors(theo,Experimental) < 1E-14)
  }
  it should "stack a vector of solutions for an A=2,2 K=3 small example" in {
    val theo = SampleDataFunctions.PostExpansion.DeltaConstraintB
    val Experimental = KSystemConstraints.deltaConstraintsb(KInputSystem)
    assert(AbsDiff.Vectors(theo,Experimental) < 1E-14)
  }
  it should "take in the A=2,2 example with K=3, and expand out an A matrix with added delta constraints" in {
    val theo = SampleDataFunctions.PostExpansion.StackedFullWithDeltasA
    val Experimental = KSystemConstraints(KInputSystem)._1
    assert(AbsDiff.Matricies(theo,Experimental) < 1E-14)
  }
  it should "Extract b for the an A=2,2 model with K=3" in {
    val Theoretical = SampleDataFunctions.PostExpansion.ExpandedBVec
    val Experimental = KSystemConstraints(KInputSystem)._2
    assert(AbsDiff.Vectors(Theoretical,Experimental) < 1E-14)
  }
}
