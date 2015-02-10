package bbdl.space
import breeze.linalg._
import breeze.numerics._
import bbdl.polytope._


/** Generates initial output */
object HelloWorld {
  def my_function(mat: DenseMatrix[Double], mat2: DenseMatrix[Double]) = {
    mat + mat2
  }
  def main(args: Array[String]) {
    println("===========================")
    val A_mini = DenseMatrix.ones[Double](3,3)
    val b_mini = DenseVector.zeros[Double](3)
    val B_mini = DenseMatrix.zeros[Double](3,3)
    //val result = myPolytopeFunction(A_mini,B_mini)
    val adder = new MyAdder()
    println(adder(1.0, 2.0))
    //println(result)
  }
}

