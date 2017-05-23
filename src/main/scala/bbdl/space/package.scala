package bbdl
import java.nio.file.{Files, Path, Paths}

import bbdl.space._
import breeze.linalg.DenseMatrix._
import breeze.linalg._
import breeze.numerics._
import breeze.math._
import breeze.stats._
import breeze.util.JavaArrayOps


/**
 * Created by Brian on 2/13/15.
 */
package object MainClass {
  def main(args: Array[String]) {
//    generate_points_toy_example_for_paper(1000000)
    generate_points_for_paracord_animation(nSamples = 100000, nSubsamples = 100, alpha_steps = 500)
  }

  def generate_points_for_paracord_animation(nSamples: Int = 100000, nSubsamples: Int = 100, alpha_steps: Int = 40): Unit = {

    val JR = DenseMatrix(
      (-0.08941, -0.0447, -0.009249, 0.03669, 0.1421, 0.2087, -0.2138),
      (-0.04689, -0.1496, 0.052,0.052, 0.0248, 0.0, 0.0248),
      (0.06472, 0.001953, -0.1518,-0.1518, 0.2919, 0.0568, 0.2067),
      (0.003081, -0.002352, -0.0001649, -0.0001649, -0.0004483, 0.0001579, -0.000685)
    )
    val Fm: DenseVector[Double] = DenseVector(123.0, 219.0,	23.52, 91.74,	21.6,	124.8,129.6)
    val H_matrix = JR*diag(Fm)
    val positive_distal_direction = DenseVector(1.0,0.0,0.0,0.0) //pure force, with no torques
    val max_out_res = MaximumOutput(H_matrix, positive_distal_direction)
    val max_force_vector = max_out_res._1
    println(max_force_vector)

    val progression_forces = linspace(0.0,0.9999999999, length= alpha_steps).map(alpha => max_force_vector*alpha)
    val array_progression_forces = progression_forces.toArray

    array_progression_forces.par.map(x => hit_run_recursive_forcevector(nSamples, nSubsamples, x, H_matrix,"finger"))

  }




  def generate_points_finger_for_paper(sample_num: Int, alpha_steps: Int): Unit = {
    val JR = DenseMatrix(
      (-0.08941, -0.0447, -0.009249, 0.03669, 0.1421, 0.2087, -0.2138),
      (-0.04689, -0.1496, 0.052,0.052, 0.0248, 0.0, 0.0248),
      (0.06472, 0.001953, -0.1518,-0.1518, 0.2919, 0.0568, 0.2067),
      (0.003081, -0.002352, -0.0001649, -0.0001649, -0.0004483, 0.0001579, -0.000685)
    )
    val Fm: DenseVector[Double] = DenseVector(123.0, 219.0,	23.52, 91.74,	21.6,	124.8,129.6)
    val H_matrix = JR*diag(Fm)
    val positive_distal_direction = DenseVector(1.0,0.0,0.0,0.0) //pure force, with no torques
    val max_out_res = MaximumOutput(H_matrix, positive_distal_direction)
    val max_force_vector = max_out_res._1
    println(max_force_vector)
    val progression_forces = linspace(0.8,0.9999999999, length= 257).map(alpha => max_force_vector*alpha)
    val array_progression_forces = progression_forces.toArray

    array_progression_forces.map(x => hit_run_recursive_forcevector(sample_num, 100, x,H_matrix,"finger"))


  }
  def generate_points_toy_example_for_paper(toy_sample_num: Int) {
    val H_inverted = DenseMatrix(
      (3.333333333),
      (-3.533333333),
      (2.0)
    )
    val H = H_inverted.t
    val positive_x_direction = DenseVector(1.0)
    //first, calculate what the maximal contraction is in the +x direction
    val max_out_res = MaximumOutput(H,positive_x_direction)
    val max_force_vector = max_out_res._1
    println(max_out_res._1)
    //scale progression vectors by the maximum force val.
    var progression_forces = linspace(0.0000001,0.9,length = 10).map(alpha => max_force_vector*alpha)
    //compute points for each of the progression forces
    progression_forces.map(x => toy_example_recursive(toy_sample_num, x))
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
    val matrix_of_feasible_x_vectors = ones[Double](n,H.cols) // prep the matrix to hold all the resultant data
    matrix_of_feasible_x_vectors(*, ::).map(dv => HitAndRun(OrthonormalBasis,StartingPoint,RandomObject))
    //HitAndRun(OrthonormalBasis,StartingPoint,RandomObject)
  }


def cadaver_experiment_H_hit_and_run(num: Int, force_vector: DenseVector[Double]) {
    import bbdl.space._
    import breeze.linalg._
    import breeze.numerics._
    import breeze.stats._
    val H_inverted = DenseMatrix(
      (-0.014591639144483588,-0.022978454210103132,-0.025904423996600777,-0.00074314929316360008,0.02905683708515799,0.039418250858721381),
      (-0.72695803910812851,-0.89355784559901341,1.3251944882192976,-0.25406166990994083,-1.6679512081270493,-0.66884960758196821),
      (0.0098418024211056644,0.014705693921162236,-0.012187842899435046,0.0015156840441345504,0.011094297953993153,0.025993177318353322),
      (-0.011308725178954909,-0.026337791625399426,-0.01759803436853305,-0.0012342879666309076,0.02759549461357709,-0.024314133219550324),
      (-0.052470830930441033,-0.062128293533437497,0.10031391656256949,0.0035509814140534685,-0.13371084553191795,-0.03093669541061039),
      (-0.035172533320395709,-0.038834705446617303,0.12056741892517414,-0.0083866339168305482,-0.17108381572684578,-0.034351131281275503),
      (-0.03434527032650516,-0.049846961079497459,0.13386517901452974,-0.0049010529463190044,-0.15254523824400101,-0.0076352171312698827)
    )
    val H = H_inverted.t
    csvwrite(new java.io.File("quickHtransposed.csv"),H)
    //val forcevector_scaled_down = forcevector*1.0
    val StartingPoint = GenStartingPoint(H,force_vector)
    println("Starting point is " +StartingPoint)
    val feasible_activations = hit_and_run_recursive_acc(Ortho(Basis(H)).toDenseMatrix, zeros[Double](1,H.cols),num,StartingPoint)
    println(
      "id174892" + " feasible activations are /n" + feasible_activations
    )
    val FileName = Output.TimestampCSVName("output/" + "toy_example_recursive" + force_vector(0) +"N_positive").toString()
    val MyFile = new java.io.File(FileName)
    csvwrite(MyFile, feasible_activations)
    println("Saved" + FileName)
  }

val toy_arm_example_H = DenseMatrix(
  (10.0/3.0, -53.0/15.0, 2.0)
)
def toy_example_recursive(num: Int, force_vector: DenseVector[Double]) {

    val H = toy_arm_example_H
    val StartingPoint = GenStartingPoint(H,force_vector)
    println("Starting point is " +StartingPoint)
    val OrthonormalBasis = Ortho(Basis(H)).toDenseMatrix
    val feasible_activations = hit_and_run_recursive_acc(OrthonormalBasis, zeros[Double](1,H.cols),num,StartingPoint)
    println(
      "id174892" + " feasible activations are /n" + feasible_activations
    )
    val FileName = Output.TimestampCSVName("output/" + "toy_example_recursive" + force_vector(0) +"N_positive_").toString()
    val MyFile = new java.io.File(FileName)
    csvwrite(MyFile, feasible_activations)
    println("Saved" + FileName)
  }

  def trim_first_row(matrix: DenseMatrix[Double]): DenseMatrix[Double] = {
    val full_length = matrix.rows
    matrix(Range(1,full_length+1),::)
  }

  def hit_run_recursive_forcevector(nSamples: Int, nSubSamples: Int, force_vector: DenseVector[Double], H_matrix: DenseMatrix[Double], plant_name: String) {

    val StartingPoint = GenStartingPoint(H_matrix,force_vector)
    val OrthonormalBasis = Ortho(Basis(H_matrix)).toDenseMatrix
    val feasible_activations = hit_and_run_recursive_acc(OrthonormalBasis, zeros[Double](1,H_matrix.cols),nSamples,StartingPoint, is_the_first_seed_point = true)

    // subsampling
    val subsampleSteps = nSamples/nSubSamples
    val slice_indices = Range(subsampleSteps,nSamples+1,subsampleSteps).toArray.map(x => x-1)
    val index_iterator_for_slices = Range(0, slice_indices.length).toArray

    // Generate matrix of subsampled activations
    val subsampled_activations = index_iterator_for_slices.map(x => feasible_activations(slice_indices(x),::).t).map(x => x.toDenseMatrix)
    val subsampled_activation_matrix = DenseMatrix.vertcat(subsampled_activations: _*)

    // output to file

    if(!Files.exists(Paths.get("output/"))) {
      Files.createDirectories(Paths.get("output"));
    }

    val FileName = Output.TimestampCSVName("output/" + plant_name + "_forcevector_" + force_vector(0) + "_").toString()
    val MyFile = new java.io.File(FileName)
    csvwrite(MyFile, subsampled_activation_matrix)
    println("Saved" + FileName)
  }

















  //important! this does not subsample, so that needs to be addressed on a higher level
  def hit_and_run_recursive_acc(OrthonormalBasis: DenseMatrix[Double],matrix_so_far: DenseMatrix[Double], iterations_remaining:Int, CurrentPoint: DenseVector[Double], is_the_first_seed_point: Boolean = false): DenseMatrix[Double] = {

    def we_have_done_enough_samples(n: Int): Boolean = {n == 0}

    if (we_have_done_enough_samples(iterations_remaining)) {
      println("finished. Length of matrix is" + matrix_so_far.rows)
      matrix_so_far
  }
    else if (is_the_first_seed_point) {
      val NewPoint = HitAndRun(OrthonormalBasis, CurrentPoint, new scala.util.Random(iterations_remaining)) //here I use the iterations as the seed
      //add point to db
      val matrix_so_far_with_new_point = NewPoint.toDenseMatrix
      //recurse now with the new point as the new seed
      hit_and_run_recursive_acc(OrthonormalBasis, matrix_so_far_with_new_point, iterations_remaining - 1, NewPoint)
    }
    else
    {
      //gen new point
      val NewPoint = HitAndRun(OrthonormalBasis, CurrentPoint, new scala.util.Random(iterations_remaining)) //here I use the iterations as the seed
      //add point to db
      val matrix_so_far_with_new_point = vertcat(matrix_so_far, NewPoint.toDenseMatrix)
      //recurse now with the new point as the new seed
      hit_and_run_recursive_acc(OrthonormalBasis, matrix_so_far_with_new_point, iterations_remaining - 1, NewPoint)
    }
  }

}

