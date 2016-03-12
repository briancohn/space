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
    println("1")
    Timing.time {
      toy_example_recursive(1, DenseVector(1.0))
    }
    println("10")
    Timing.time {
      toy_example_recursive(10, DenseVector(1.0))
    }
  }


  val toy_arm_example_H = DenseMatrix(
    (10.0 / 3.0, -53.0 / 15.0, 2.0)
  )

  def toy_example_recursive(num: Int, force_vector: DenseVector[Double]) {
    val autocorrelation_jumps = 100
    val H = toy_arm_example_H
    val b_vector = DenseVector(1.0)
    MixingAlgorithm.uar_point(autocorrelation_jumps, toy_arm_example_H, b_vector)
    //    val FileName = Output.TimestampCSVName("output/" + "toy_example_HR" + force_vector(0) +"N_positive").toString()
    //    val MyFile = new java.io.File(FileName)
    //    csvwrite(MyFile, uar_points)
    //    println("Saved " + FileName)
  }
}

