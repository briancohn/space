package bbdl.space
import breeze.linalg._
import breeze.numerics._

class myFunction extends Function2[DenseMatrix[Double], DenseMatrix[Double], DenseMatrix[Double]] with Serializable {
  def apply(a: DenseMatrix[Double], b: DenseMatrix[Double]): DenseMatrix[Double] =
    a + b
}
 