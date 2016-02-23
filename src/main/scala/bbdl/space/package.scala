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
//    PointsFor(PointsPerAlpha, DenseVector(0.0,0.0,1.0,0.0), "Z", 10, Tuple2(0.1,1.0)) //xy direction
    cadaveric_main(1000)
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
      (0.003081, -0.002352, -0.0001649, -0.0001649, -0.0004483, 0.0001579, -0.000685)
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
//  n is the number of hit and run points to generate
def cadaveric_main(num: Int) {
  import bbdl.space._
  import breeze.linalg._
  import breeze.numerics._
  import breeze.stats._
  println("Welcome to Brian's Hit and Run")
  println("Currently Pointpicking")
  val H_inverted = DenseMatrix(
    (-0.0293685254,-0.00668443263,-0.0559388282,-0.00293887393,-0.00271804379,0.0675979006),
    (0.0524124522,0.0085659928,-0.0863923627,0.0044785397,0.0166977331,-0.000177014528),
    (-0.000343820587,-0.0000483876833,-0.000238701488,0.0000795781151,0.000367717412,-0.00152154611),
    (-0.00104260042,-0.000117578104,0.00217179502,0.0000399547137,0.000697037766,-0.00563909685),
    (-0.000635105145,-0.0000651667976,0.00126569739,0.000178511441,0.000279314307,-0.00284169699),
    (0.0000563817059,-0.0000611529608,-0.00362413553,0.000341519435,0.000242187881,0.0025295333),
    (-0.00101664236,0.0000392363207,0.0143094685,0.00014002014,-0.00317811064,0.0018533981)
  )
  val H = H_inverted.t

  println("H Matrix load SUCCESS")
  val forcevector = DenseVector(-0.0293685254,-0.00668443263,-0.0559388282,-0.00293887393,-0.00271804379,0.0675979006)
  val forcevector_scaled_down = forcevector*1.0
  println("Vector load SUCCESS")
  val feasible_activations = cadaveric_hit_and_run(H, forcevector_scaled_down, num)
  println(feasible_activations)
  println("Process Completed. Exiting")
}

  def cadaveric_hit_and_run(H: DenseMatrix[Double], forcevector: DenseVector[Double], n: Int): DenseMatrix[Double] ={
    import bbdl.space._
    import breeze.linalg._
    val RandomObject = new scala.util.Random(10)
    val Basis_H = Basis(H)
    val OrthonormalBasis = Ortho(Basis_H  ).toDenseMatrix
    val StartingPoint = GenStartingPoint(H,forcevector)
    PointStream.generate(n,OrthonormalBasis,StartingPoint,RandomObject)
  }
}

