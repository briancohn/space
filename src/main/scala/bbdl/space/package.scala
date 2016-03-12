package bbdl
import bbdl.space._
import scala.annotation.tailrec
import breeze.linalg._
import breeze.numerics._
import breeze.math._
import breeze.stats._
/**
 * Created by Brian on 2/13/15.
 */
package object MainClass {
  def main(args: Array[String]) {
    println("100000")
    Timing.time {
//      toy_example_recursive(100000, DenseVector(1.0), 2)
      fco_finger_recursive(100, DenseVector(1.0,1.0,0.0,0.0), 2)
    }
  }


  val toy_arm_example_H = DenseMatrix(
    (10.0 / 3.0, -53.0 / 15.0, 2.0)
  )

  val francisco_JR = DenseMatrix(
    (-0.08941, -0.0447, 0.2087, -0.2138, -0.009249, 0.1421, 0.03669),
    (-0.04689, -0.1496, 0.0, 0.0248, 0.052, 0.0248, 0.052),
    (0.06472, 0.001953, 0.0568, 0.2067, -0.1518, 0.2919, -0.1518),
    (0.003081, -0.002352, 0.0001578, -0.000685, -0.0001649, -0.0004483, -0.0001649)
  )
  val francisco_maximum_forces_across_muscles = DenseVector(123,219,124.8,129.6,23.52,21.6,91.74)

  val francisco_H = francisco_JR*diag(francisco_maximum_forces_across_muscles)

  def toy_example_recursive(num: Int, force_vector: DenseVector[Double], seed: Int) {
    val autocorrelation_jumps = 100
    val H = toy_arm_example_H
    val b_vector = DenseVector(1.0)
    val points = MixingAlgorithm.uar_points(autocorrelation_jumps, H, b_vector, num)
    val FileName = Output.TimestampCSVName("output/" + "toy_example_HR" + force_vector(0) +"N_positive").toString()
    val MyFile = new java.io.File(FileName)
    csvwrite(MyFile, points)
    println("Saved " + FileName)
  }

  def fco_finger_recursive(num: Int, force_vector: DenseVector[Double], seed: Int) {
    val autocorrelation_jumps = 100
    val H = francisco_H
    val points = MixingAlgorithm.uar_points(autocorrelation_jumps, H, force_vector, num)
    val FileName = Output.TimestampCSVName("output/" + "fco_finger_HR" + force_vector(0) +"XY").toString()
    val MyFile = new java.io.File(FileName)
    csvwrite(MyFile, points)
    println("Saved " + FileName)
  }
}

