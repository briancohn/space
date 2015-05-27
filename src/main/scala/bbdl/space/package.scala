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
    var PointsPerAlpha = 1000 //default value
    if(args.length==1) {
      PointsPerAlpha = args(0).toInt
    }
    println("Currently Pointpicking")
    PointsFor(PointsPerAlpha, DenseVector(0.0,0.0,1.0,0.0), "Z", 10, Tuple2(0.1,1.0)) //xy direction
    println("done w Z")
  }
  def PointsFor(PointsPerAlpha: Int, v:DenseVector[Double], direction: String, AlphaLenOut:Int, AlphaLim: Tuple2[Double,Double]): Unit ={
    import bbdl.space._
    import breeze.linalg._
    import breeze.numerics._
    import breeze.stats._
    val Seed = 10
    val RandomObject = new scala.util.Random(Seed)
    val JR = DenseMatrix(
      (-0.08941, -0.0447, -0.009249, 0.03669, 0.1421, 0.2087, -0.2138),
      (-0.04689, -0.1496, 0.052,0.052, 0.0248, 0.0, 0.0248),
      (0.06472, 0.001953, -0.1518,-0.1518, 0.2919, 0.0568, 0.2067),
      (0.003081, -0.002352, -0.0001649, -0.0001649, -0.0004483, 0.0001578, -0.000685)
    )
    val Fm: DenseVector[Double] = DenseVector(123.0, 219.0,	23.52, 91.74,	21.6,	124.8,129.6)
    val A = JR*diag(Fm)
    val db = PointStream.alphaGenerate(PointsPerAlpha, AlphaLim, AlphaLenOut, v, A, Ortho(Basis(A)), RandomObject)
    val DBwithCosts = Cost.GenCosts(db, A.cols, Fm)
    val FileName = Output.TimestampCSVName("output/"+ direction + "_alphaProgression").toString()
    val MyFile = new java.io.File(FileName)
    println("Saving to " + FileName)
    csvwrite(MyFile, DBwithCosts)
  }
}