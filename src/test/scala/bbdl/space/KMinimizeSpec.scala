package bbdl.space

import bbdl.space.SampleDataFunctions.PreExpansion._
import breeze.linalg.DenseVector
import org.scalatest.{Matchers, FlatSpec}

/**
 *
Created by B
 Time: 11:18 AM, 5/26/2015;
 Project: space
 */
class KMinimizeSpec() extends FlatSpec with Matchers{
 "Minimizing sum of sum of activation vectors" should "produce the minimal sum of sum of activation solution for a k3n2 example" in {
  val KMini = SampleDataFunctions.K3N2Mini.KSystemsMini
  val TheoreticalX = DenseVector(0.3939393939393939, 0.33787878787878783, 0.593939393939394, 0.37575757575757573, 0.3939393939393939, 0.5757575757575757)
  val ExperimentalX = KMinimize.SumOfSumOfActivations(KMini)
  assert(AbsDiff.Vectors(TheoreticalX,ExperimentalX) < 1E-14)
 }
 "Minimizing sum of sum of activation vectors" should "when delta constraints are removed, see expected unconstrained solutions" in {
  val KMiniNoDelta = SampleDataFunctions.K3N2NoDelta.KSystemsMini
  val TheoreticalX = DenseVector(0.0,0.2,0.5,0.0,0.0,0.3)
  val ExperimentalX = KMinimize.SumOfSumOfActivations(KMiniNoDelta)
  assert(AbsDiff.Vectors(TheoreticalX,ExperimentalX) < 1E-14)
 }
 "Minimizing the sum of change in activation from j to j+1" should "produce the minimal result" in {
  val KMini = SampleDataFunctions.K3N2Mini.KSystemsMini
  val ExperimentalX = KMinimize.AbsDiffOfSumOfDeltaActivation(KMini)
  val TheoreticalX = DenseVector(0.6363636363636364, 0.4227272727272727, 0.6363636363636365, 0.5454545454545454, 0.6363636363636364, 0.7454545454545455, 0.0, 0.12272727272727275, 0.0, 0.2)
  assert(AbsDiff.Vectors(TheoreticalX,ExperimentalX) < 1E-14)
 }
}