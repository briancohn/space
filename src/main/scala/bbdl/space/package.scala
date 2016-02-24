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
    val vector_progression = linspace(-3.5333,5.3333,6)
    println("Beginning Vector progression on Toy Example.")
//    vector_progression.map(v => toy_example(10000, DenseVector(v)))
    vector_progression.map(v => toy_example_recursive(10000, DenseVector(v)))

    println("done w toy example")
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
def toy_example(num: Int, vector: DenseVector[Double]) {
  import bbdl.space._
  import breeze.linalg._
  import breeze.numerics._
  import breeze.stats._
  val H_inverted = DenseMatrix(
    (3.333333333),
    (-3.533333333),
    (2.0)
  )
  val H = H_inverted.t
  //val forcevector_scaled_down = forcevector*1.0
  val feasible_activations = hit_and_run_repetitions(H, vector, num)
  val FileName = Output.TimestampCSVName("output/" + "toy_example_" + vector(0) +"N_positive").toString()
  val MyFile = new java.io.File(FileName)
  csvwrite(MyFile, feasible_activations)
  println("Saved" + FileName)
}

  def hit_and_run_repetitions(H: DenseMatrix[Double], forcevector: DenseVector[Double], n: Int): DenseMatrix[Double] ={
    import bbdl.space._
    import breeze.linalg._
    val RandomObject = new scala.util.Random(10)
    val Basis_H = Basis(H)
    val OrthonormalBasis = Ortho(Basis_H).toDenseMatrix
    val StartingPoint = GenStartingPoint(H,forcevector)
    val matrix_of_feasible_x_vectors = DenseMatrix.ones[Double](n,H.cols) // prep the matrix to hold all the resultant data
    matrix_of_feasible_x_vectors(*, ::).map(dv => HitAndRun(OrthonormalBasis,StartingPoint,RandomObject))
    //HitAndRun(OrthonormalBasis,StartingPoint,RandomObject)
  }


  def toy_example_recursive(num: Int, force_vector: DenseVector[Double]) {
    import bbdl.space._
    import breeze.linalg._
    import breeze.numerics._
    import breeze.stats._
    val H_inverted = DenseMatrix(
      (3.333333333),
      (-3.533333333),
      (2.0)
    )
    val H = H_inverted.t
    //val forcevector_scaled_down = forcevector*1.0
    val StartingPoint = GenStartingPoint(H,force_vector)
    val feasible_activations = hit_and_run_recursive_acc(Ortho(Basis(H)).toDenseMatrix, DenseMatrix.zeros[Double](1,H.cols),num,StartingPoint)
    println(
      "id174892" + " feasible activations are /n" + feasible_activations
    )
    val FileName = Output.TimestampCSVName("output/" + "toy_example_recursive" + force_vector(0) +"N_positive").toString()
    val MyFile = new java.io.File(FileName)
    csvwrite(MyFile, feasible_activations)
    println("Saved" + FileName)
  }


  //important! this does not subsample,so that needs to be addressed on a higher level
  def hit_and_run_recursive_acc(OrthonormalBasis: DenseMatrix[Double],matrix_so_far: DenseMatrix[Double], iterations_remaining:Int, CurrentPoint: DenseVector[Double]): DenseMatrix[Double] = {
    def we_have_done_enough_samples(n: Int): Boolean = {n == 0}
    if (we_have_done_enough_samples(iterations_remaining)) {
      println("finished. Length of matrix is" + matrix_so_far.rows)
      matrix_so_far
  }
    else {
      //gen new point
      println("Iteration_lim_not_met. Picking another point.")
      println("Orthonormal Basis is" + OrthonormalBasis)
      println("Current Point is" + CurrentPoint)
      val NewPoint = HitAndRun(OrthonormalBasis, CurrentPoint, new scala.util.Random(iterations_remaining)) //here I use the iterations as the seed
      //add point to db
      val matrix_so_far_with_new_point = DenseMatrix.vertcat(matrix_so_far, NewPoint.toDenseMatrix)
      println("NewPoint. Iterations remaining = " + iterations_remaining)
      hit_and_run_recursive_acc(OrthonormalBasis, matrix_so_far_with_new_point, iterations_remaining - 1, NewPoint)
    }
  }

}

