package bbdl
import bbdl.space._
import breeze.linalg._
import breeze.numerics._
import breeze.math._
/**
 * Created by Brian on 2/13/15.
 */
package object MainClass {
  def main(args: Array[String]) {
    println("Hello, world!")
    val PointsPerAlpha = args(0).toInt
    println("Currently Pointpicking")
    PointsFor(PointsPerAlpha, DenseVector(1.0,0.0,0.0,0.0), "X") //x direction
    println("done w x")
    PointsFor(PointsPerAlpha, DenseVector(0.0,1.0,0.0,0.0), "Y") //y direction
    println("done w y")
    PointsFor(PointsPerAlpha, DenseVector(1.0,1.0,0.0,0.0), "XY") //xy direction
    println("done w xy")
  }
  def PointsFor(PointsPerAlpha: Int, v:DenseVector[Double], direction: String): Unit ={
    import bbdl.space._
    import breeze.linalg._
    import breeze.numerics._
    import breeze.stats._
    val Seed = 10
    val RandomObject = new scala.util.Random(Seed)
    val JR = DenseMatrix(
      (-0.08941, -0.0447, 0.2087, -0.2138, -0.009249, 0.1421, 0.03669),
      (-0.04689, -0.1496, 0.0, 0.0248, 0.052, 0.0248, 0.052),
      (0.06472, 0.001953, 0.0568, 0.2067, -0.1518, 0.2919, -0.1518),
      (0.003081, -0.002352, 0.0001578, -0.000685, -0.0001649, -0.0004483, -0.0001649)
    )
    val Fm = DenseVector(123,219,124.8,129.6,23.52,21.6,91.74)
    val A = JR*diag(Fm)
    val OrthonormalBasis = Ortho(Basis(A)) //Orthogonalize the basis
    val AlphaLenOut = 10
    val db = PointStream.alphaGenerate(PointsPerAlpha, Tuple2(0.0, 0.9), AlphaLenOut, v, A, OrthonormalBasis, RandomObject)
    val DBwithCosts = Cost.GenCosts(db, A.cols, Fm)
    val FileName = Output.TimestampCSVName("output/"+ direction + "_alphaProgression").toString()
    val MyFile = new java.io.File(FileName)
    println("Saving to " + FileName)
    csvwrite(MyFile, DBwithCosts)
  }
}