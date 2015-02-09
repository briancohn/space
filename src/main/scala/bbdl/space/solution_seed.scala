package bbdl.space
import breeze.linalg._
import breeze.numerics._

class MyFunction extends Function2[DenseMatrix[Double], DenseMatrix[Double], DenseMatrix[Double]] extends Serializable {
  def apply(a: DenseMatrix[Double], b: DenseMatrix[Double]): DenseMatrix[Double] =
    a + b
}
