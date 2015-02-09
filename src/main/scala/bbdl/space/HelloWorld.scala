package bbdl.space
import breeze.linalg._
import breeze.numerics._
/** Generates initial output */
object HelloWorld {
  def my_function(mat: DenseMatrix[Double], mat2: DenseMatrix[Double]) = {
    mat + mat2
    
  }
  def main(args: Array[String]) {
    println("===========================")
    val A_mini = DenseMatrix.ones[Double](3,3)
    val b_mini = DenseVector.zeros[Double](3)
    println(A_mini)
    println(b_mini)
    println(my_function(A_mini, A_mini))
    quickF = MyFunction
    println("===========================")
  }
}

