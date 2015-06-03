package bbdl.space
import java.io.File
import breeze.linalg._
import breeze.numerics._
import breeze.math._
import org.scalatest._
import Matchers._
import scala.collection.JavaConverters._
import scala.util.Random

object SampleDataFunctions {
  val thearray = Array(PreExpansion.SystemMini1, PreExpansion.SystemMini2, PreExpansion.SystemMini3)

  def K3Systems = KGeneratorSystems(thearray)

  object PreExpansion {
    def A1 = DenseMatrix((1.0, 2.0), (3.0, 4.0))

    def A2 = DenseMatrix((5.0, 6.0), (7.0, 8.0))

    def A3 = DenseMatrix((9.0, 10.0), (11.0, 12.0))

    def ShortAList = Array(A1, A2, A3)

    def b1 = DenseVector((1.0), (2.0))

    def b2 = DenseVector((3.0), (4.0))

    def b3 = DenseVector((5.0), (6.0))

    def deltas1 = DenseVector((0.1), (0.2))

    def deltas2 = DenseVector((0.3), (0.4))

    def deltas3 = DenseVector((0.5), (0.6))

    def ShortbList = Array(b1, b2, b3)

    def KSystems = ShortAList.zip(ShortbList)

    def SystemMini1 = GeneratorSystem(PreExpansion.A1, PreExpansion.b1, PreExpansion.deltas1)

    def SystemMini2 = GeneratorSystem(PreExpansion.A2, PreExpansion.b2, PreExpansion.deltas2)

    def SystemMini3 = GeneratorSystem(PreExpansion.A3, PreExpansion.b3, PreExpansion.deltas3)

    def KSystemsMini = KGeneratorSystems(Array(SystemMini1, SystemMini2, SystemMini3))
  }

  object K3N2Mini {
    def A1 = DenseMatrix((-0.35, 1.0))

    def A2 = DenseMatrix((-4.0,1.0))

    def A3 = DenseMatrix((-0.7,1.0))

    def b1 = DenseVector((0.2))

    def b2 = DenseVector((-2.0))

    def b3 = DenseVector((0.3))

    def deltas1 = DenseVector((0.2), (0.2))

    def deltas2 = DenseVector((0.2), (0.2))

    def deltas3 = DenseVector((0.2), (0.2))

    def ShortbList = Array(b1, b2, b3)

    def SystemMini1 = GeneratorSystem(A1, b1, deltas1)

    def SystemMini2 = GeneratorSystem(A2, b2, deltas2)

    def SystemMini3 = GeneratorSystem(A3, b3, deltas3)

    def KSystemsMini = KGeneratorSystems(Array(SystemMini1, SystemMini2, SystemMini3))
  }
  object K4N2Mini{
    def KSystemsMini = KGeneratorSystems(Array(K3N2Mini.SystemMini1, K3N2Mini.SystemMini2,K3N2Mini.SystemMini3,K3N2Mini.SystemMini3))
  }

  object K2N2SystemsMini {
    def apply = KGeneratorSystems(Array(K3N2Mini.SystemMini1, K3N2Mini.SystemMini2))
  }
  object K3N2NoDelta {
    def A1 = DenseMatrix((-0.35, 1.0))

    def A2 = DenseMatrix((-4.0,1.0))

    def A3 = DenseMatrix((-0.7,1.0))

    def b1 = DenseVector((0.2))

    def b2 = DenseVector((-2.0))

    def b3 = DenseVector((0.3))

    def deltas1 = DenseVector((1.0), (1.0))

    def deltas2 = DenseVector((1.0), (1.0))

    def deltas3 = DenseVector((1.0), (1.0))

    def ShortbList = Array(b1, b2, b3)

    def SystemMini1 = GeneratorSystem(A1, b1, deltas1)

    def SystemMini2 = GeneratorSystem(A2, b2, deltas2)

    def SystemMini3 = GeneratorSystem(A3, b3, deltas3)

    def KSystemsMini = KGeneratorSystems(Array(SystemMini1, SystemMini2, SystemMini3))
  }

  object PostExpansion {
    def paddedA1 = DenseMatrix((1.0, 2.0, 0.0, 0.0, 0.0, 0.0),
      (3.0, 4.0, 0.0, 0.0, 0.0, 0.0),
      (-1.0, -2.0, 0.0, 0.0, 0.0, 0.0),
      (-3.0, -4.0, 0.0, 0.0, 0.0, 0.0))

    def paddedA2 = DenseMatrix((0.0, 0.0, 5.0, 6.0, 0.0, 0.0),
      (0.0, 0.0, 7.0, 8.0, 0.0, 0.0),
      (0.0, 0.0, -5.0, -6.0, 0.0, 0.0),
      (0.0, 0.0, -7.0, -8.0, 0.0, 0.0))

    def paddedA3 = DenseMatrix((0.0, 0.0, 0.0, 0.0, 9.0, 10.0),
      (0.0, 0.0, 0.0, 0.0, 11.0, 12.0),
      (0.0, 0.0, 0.0, 0.0, -9.0, -10.0),
      (0.0, 0.0, 0.0, 0.0, -11.0, -12.0))

    def KStackedConstraints = DenseMatrix.vertcat(Array(paddedA1, paddedA2, paddedA3):_*)

    def ConstrainToPositive = DenseMatrix(
      (1.0, 0.0, 0.0, 0.0, 0.0, 0.0),
      (0.0, 1.0, 0.0, 0.0, 0.0, 0.0),
      (0.0, 0.0, 1.0, 0.0, 0.0, 0.0),
      (0.0, 0.0, 0.0, 1.0, 0.0, 0.0),
      (0.0, 0.0, 0.0, 0.0, 1.0, 0.0),
      (0.0, 0.0, 0.0, 0.0, 0.0, 1.0))

    def ConstrainBelow1 = DenseMatrix(
      (-1.0, 0.0, 0.0, 0.0, 0.0, 0.0),
      (0.0, -1.0, 0.0, 0.0, 0.0, 0.0),
      (0.0, 0.0, -1.0, 0.0, 0.0, 0.0),
      (0.0, 0.0, 0.0, -1.0, 0.0, 0.0),
      (0.0, 0.0, 0.0, 0.0, -1.0, 0.0),
      (0.0, 0.0, 0.0, 0.0, 0.0, -1.0)
    )

    def StepDeltaChangeConstraint = DenseMatrix(
      (1.0, 0.0, -1.0, 0.0, 0.0, 0.0),
      (0.0, 1.0, 0.0, -1.0, 0.0, 0.0),
      (0.0, 0.0, 1.0, 0.0, -1.0, 0.0),
      (0.0, 0.0, 0.0, 1.0, 0.0, -1.0),
      (-1.0, 0.0, 1.0, 0.0, 0.0, 0.0),
      (0.0, -1.0, 0.0, 1.0, 0.0, 0.0),
      (0.0, 0.0, -1.0, 0.0, 1.0, 0.0),
      (0.0, 0.0, 0.0, -1.0, 0.0, 1.0)
    )

    def ExpandedBVec = {
      val Bs = DenseVector.vertcat(Array(PreExpansion.b1,-PreExpansion.b1,PreExpansion.b2,-PreExpansion.b2,PreExpansion.b3,-PreExpansion.b3):_*)
      val Bounds = DenseVector.vertcat(DenseVector.zeros[Double](6), DenseVector.ones[Double](6))
      val DeltaConstraints = DenseVector(0.1,0.2,0.3,0.4)
      DenseVector.vertcat(Array(Bs,Bounds,DeltaConstraints, DeltaConstraints):_*)
    }
    def StackedFullWithDeltasA = DenseMatrix.vertcat(Array(KStackedConstraints, ConstrainBelow1,ConstrainToPositive,StepDeltaChangeConstraint):_*)

    def DeltaConstraintB = DenseVector.vertcat(Array(PreExpansion.deltas1, PreExpansion.deltas2, PreExpansion.deltas1, PreExpansion.deltas2):_*)

    def apply(): DenseMatrix[Double] = {
      val MatList = List(paddedA1, paddedA2, paddedA3, ConstrainToPositive, ConstrainBelow1, StepDeltaChangeConstraint)
      DenseMatrix.vertcat(MatList: _*)
    }



  }

}