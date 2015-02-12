package bbdl.polytope
import breeze.linalg._
import breeze.numerics._

class MyAdder extends Function2[Double, Double, Double] {
	def apply(a: Double, b: Double) = {
		a + b
	}
}

class get_random_direction {
	def apply(B: DenseMatrix[Double], v: DenseVector[Double]) = {
		0
	}

}
object extract_right_square {
	def apply(M: DenseMatrix[Double]) = {
		val row_num = M.rows //m
		val col_num = M.cols //n
		val start_col = col_num-row_num
		var output = M(::, start_col)
		for( col <- start_col+1 to col_num-1) {
			var new_generated_col = M(::, col)
			output = DenseMatrix.horzcat(output.toDenseVector,new_generated_col.toDenseVector)
		}
		println(output)
		output
		}
}

object insert_col {
	def apply(M: DenseMatrix[Double], v: DenseVector[Double], k: Int) = {
		for( row_num <- 0 to M.rows) {
			M(row_num, k) = v(row_num)
		}
		M
	}
	
}

// @return B DenseMatrix[Double] basis vectors matrix, with each column representing a basis vector.
class basis{
	def apply(generators: DenseMatrix[Double]) = {
		val row_num = generators.rows
		val col_num = generators.cols
		var basis_solutions = DenseMatrix.zeros[Double](row_num, col_num-row_num)
		if (row_num > col_num)
    		println("You need to input a matrix where the number of columns is equal to or higher than the number of rows.")
		var current_col_to_insert = 0 
		val list = List.range(0, col_num-row_num-1) 
		for(i  <- list) {
			val right_square = extract_right_square(generators)
			val current_col = generators(::,i)
			val x = right_square \ -current_col
			val current_solutions = x(1 to -1) //exclude the first element, which we know is a one.
			basis_solutions = insert_col(basis_solutions, current_solutions, current_col_to_insert)
			current_col_to_insert = current_col_to_insert + 1
		}
		//generate a square matrix with ones down the middle, to represent each of the basis vectors we solved for.
		val diagonal_mat = DenseMatrix.eye[Double](col_num-row_num)
		DenseMatrix.vertcat(diagonal_mat,basis_solutions)
	}

}
	


class orth {
	def apply(a: DenseMatrix[Double]) = {
		val m = a.rows
		val n = a.cols
		var basis_vector = a(::, 0)
		var b = DenseMatrix.create[Double](m, 1, (basis_vector / norm(basis_vector)).toArray)
		for (i <- 1 until n) {
		  basis_vector = a(::, i)
		  basis_vector = basis_vector - b * (b.t * basis_vector)
		  b = DenseMatrix.horzcat(b, DenseMatrix.create[Double](m, 1, basis_vector.toArray))
		}
		b
	} 
}
