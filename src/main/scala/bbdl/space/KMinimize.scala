package bbdl.space

import breeze.linalg.DenseVector

/**
 *
Created by B
 Time: 11:17 AM, 5/26/2015;
 Project: space
 */
object KMinimize {
 def SumOfSumOfActivations(kGeneratorSystems: KGeneratorSystems): DenseVector[Double] = {
  val K = kGeneratorSystems.KSystemArray.length
  val n = kGeneratorSystems.KSystemArray(0).A.cols
  val DeltaConstraints = KSystemConstraints(kGeneratorSystems)
  val A = DeltaConstraints._1
  val b = DeltaConstraints._2
  val c = -DenseVector.ones[Double](K*n)
  val x = LowLevelSimplex(A,b,c)
  x
 }
 def AbsDiffOfSumOfDeltaActivation(kGeneratorSystems: KGeneratorSystems): DenseVector[Double] = {
  val K = kGeneratorSystems.KSystemArray.length
  val n = kGeneratorSystems.KSystemArray(0).A.cols
  val AVDelta = KSystemConstraintsAbsDiffDelta.apply(kGeneratorSystems)
  val A = AVDelta._1
  val b = AVDelta._2
  val KConstraintVariables = DenseVector.zeros[Double](K*n)
  val AuxiliaryVariables = -DenseVector.ones[Double](n*(K-1))
  val c = DenseVector.vertcat(KConstraintVariables, AuxiliaryVariables)
  val x = LowLevelSimplex(A, b, c)
  x
 }
}